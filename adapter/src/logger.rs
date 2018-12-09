use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync;

struct FileLogger {
    file: io::BufWriter<fs::File>,
}

impl FileLogger {
    fn create(file_path: &Path) -> io::Result<FileLogger> {
        let file = fs::File::create(file_path)?;
        let file = io::BufWriter::new(file);
        Ok(FileLogger { file })
    }

    fn flush(&mut self) {
        self.file.flush().ok();
    }
}

enum LazyInit<T> {
    /// 未初期化。
    Uninit,
    /// 破棄済み。
    Deinit,
    /// 初期化済み。
    Value(T),
}

static mut LOGGER: Option<sync::Mutex<LazyInit<FileLogger>>> = None;

/// モジュールの初期化処理を行う。
pub(crate) fn initialize_mod() {
    unsafe { LOGGER = Some(sync::Mutex::new(LazyInit::Uninit)) };
}

/// モジュールの終了時の処理を行う。
pub(crate) fn deinitialize_mod() {
    (|| {
        log("デバッガーがデタッチされました");

        let mutex = unsafe { LOGGER.as_ref() }?;
        let mut lock = mutex.lock().ok()?;
        if let LazyInit::Value(ref mut logger) = *lock {
            logger.flush();
        }
        *lock = LazyInit::Deinit;
        Some(())
    })();
}

/// ロガーを使った処理を行う。
fn with_logger<F>(f: F)
where
    F: Fn(&mut FileLogger),
{
    (|| {
        // NOTE: static mut 変数へのアクセスは unsafe 。
        let logger_mutex: &sync::Mutex<_> = unsafe { LOGGER.as_ref() }?;

        // ロガーの所有権を一時的に借用する。
        let mut logger_lock = logger_mutex.lock().ok()?;

        // 初めてロガーを使用するときのみ、初期化を行う。
        if let LazyInit::Uninit = *logger_lock {
            let logger = match FileLogger::create(&log_file_path()) {
                Ok(logger) => LazyInit::Value(logger),
                Err(_) => LazyInit::Deinit,
            };
            *logger_lock = logger;
        }

        match *logger_lock {
            LazyInit::Uninit => unreachable!(),
            LazyInit::Deinit => {}
            LazyInit::Value(ref mut l) => f(l),
        }

        Some(())
    })();
}

#[allow(deprecated)]
fn log_file_path() -> PathBuf {
    std::env::home_dir()
        .map(|d| d.join("hsp3debug-rust.log"))
        .unwrap()
}

pub(crate) fn log(message: &str) {
    with_logger(|logger| {
        writeln!(logger.file, "{}", message).unwrap();
    })
}

pub(crate) fn log_error<E: std::fmt::Debug>(err: &E) {
    let message = format!("[ERROR] {:?}", err);
    log(&message)
}
