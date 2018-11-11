#include "stdafx.h"
#include "hspsdk/hsp3debug.h"
#include "hspsdk/hsp3struct.h"
#include <array>
#include <cassert>
#include <cctype>
#include <fstream>
#include <vector>

#define EXPORT extern "C" __declspec (dllexport)
#define BOOL int

typedef BOOL (WINAPI* DebugInitFn)(HSP3DEBUG* p1, int p2, int p3, int p4);
typedef BOOL (WINAPI* DebugNoticeFn)(HSP3DEBUG* p1, int p2, int p3, int p4);

auto trim_end(std::string& s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](char ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

auto ends_with(std::wstring const& str, std::wstring const& suffix) {
    if (str.length() < suffix.length()) {
        return false;
    }
    return std::equal(str.end() - suffix.length(), str.end(), suffix.begin(), suffix.end());
}

auto strip_suffix(std::wstring const& str, std::wstring const& suffix) {
    assert(ends_with(str, suffix));
    return std::wstring{ str.begin(), str.end() - suffix.length() };
}

auto utf8_to_wide_string(std::string const& str) {
    // バッファサイズを計算する。終端文字を含まれないので注意。
    auto size = 1 + MultiByteToWideChar(CP_UTF8, 0, str.c_str(), str.length(), nullptr, 0);
    assert(size > 1);

    auto buf = std::vector<wchar_t>();
    buf.resize(size, L'\0');
    auto code = MultiByteToWideChar(CP_UTF8, 0, str.c_str(), str.length(), buf.data(), buf.size());
    assert(code != 0);

    return std::wstring{ buf.data() };
}

[[noreturn]]
auto fail_with(std::wstring const& str) -> void {
    MessageBox(nullptr, str.c_str(), L"hsp3debug", MB_OK);
    std::abort();
}

struct ModuleCloseFn
{
    void operator()(HMODULE h) const
    {
        FreeLibrary(h);
    }
};

using ModuleHandle = std::unique_ptr<std::remove_pointer<HMODULE>::type, ModuleCloseFn>;

auto load_library(std::wstring const& full_path) {
    auto handle = LoadLibrary(full_path.c_str());
    if (handle == nullptr) {
        fail_with(L"Couldn't load library " + full_path);
    }

    return ModuleHandle{ handle };
}

class DebugLibrary {
public:
    ModuleHandle handle;
    DebugInitFn debugini;
    DebugNoticeFn debug_notice;

    operator bool() {
        return !!handle;
    }
};

auto load_debug_library(std::wstring const& full_path) {
    auto handle = load_library(full_path);
    auto debugini = (DebugInitFn)GetProcAddress(handle.get(), "_debugini@16");
    auto debug_notice = (DebugInitFn)GetProcAddress(handle.get(), "_debug_notice@16");
    return DebugLibrary{
        std::move(handle),
        debugini,
        debug_notice,
    };
}

static auto attach_debugger(std::wstring const& dir_name) -> DebugLibrary {
    auto f = std::ifstream{ dir_name + L"hsp3debug.txt" };
    if (!f.is_open()) {
        fail_with(L"Make hsp3debug.txt and write debugger file name to load.");
    }

    auto path = std::string{
        std::istreambuf_iterator<char>{f},
        std::istreambuf_iterator<char>{},
    };
    trim_end(path);
    auto wpath = utf8_to_wide_string(path);
    auto full_path = dir_name + wpath;

    return load_debug_library(full_path);
}

static DebugLibrary s_lib{};

BOOL APIENTRY DllMain(
    HMODULE h_module,
    DWORD  ul_reason_for_call,
    LPVOID reserved
)
{
    switch (ul_reason_for_call)
    {
        case DLL_PROCESS_ATTACH: {
            MessageBox(nullptr, L"attach!", L"hsp3debug", MB_OK);
            OutputDebugString(TEXT("attach!"));

            auto buf = std::array<wchar_t, MAX_PATH>();
            GetModuleFileName(h_module, buf.data(), buf.size());
            auto file_name = std::wstring{ buf.data() };
            auto dir_name = strip_suffix(file_name, std::wstring{ L"hsp3debug.dll" });

            auto lib = attach_debugger(dir_name);
            std::swap(s_lib, lib);
            break;
        }
        case DLL_THREAD_ATTACH:
        case DLL_THREAD_DETACH:
            break;
        case DLL_PROCESS_DETACH: {
            OutputDebugString(TEXT("detach!"));
            auto lib = DebugLibrary{};
            std::swap(s_lib, lib);
            break;
        }
    }
    return TRUE;
}

// デバッガーがアタッチされたときに HSP ランタイムから呼ばれる。
EXPORT BOOL APIENTRY debugini(HSP3DEBUG* p1, int p2, int p3, int p4)
{
    if (!s_lib || !s_lib.debug_notice) {
        fail_with(L"Couldn't not load debugini from the library.");
    }

    (s_lib.debugini)(p1, p2, p3, p4);
    return 0;
}

// logmes や assert 0 が実行されたときに HSP ランタイムから呼ばれる。
EXPORT BOOL WINAPI debug_notice(HSP3DEBUG* p1, int p2, int p3, int p4)
{
    if (!s_lib || !s_lib.debug_notice) {
        fail_with(L"Couldn't not load debug_notice from the library.");
    }

    (s_lib.debug_notice)(p1, p2, p3, p4);
    return 0;
}
