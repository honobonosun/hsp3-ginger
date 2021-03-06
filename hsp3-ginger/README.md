# GINGER コマンド

`ginger` コマンドは、コマンドラインで HSP3 スクリプトのコンパイルや実行を行うためのユーティリティーです。

## インストール

[ginger.exe をダウンロード](https://github.com/vain0x/hsp3-ginger/raw/master/hsp3-ginger/bin/ginger.exe) して、HSP3 のインストールディレクトリに配置してください。(HSP3 のコンパイラである `hspcmp.dll` を使用するため、他のディレクトリには配置できません。)

パスを通すかエイリアスを張ると便利です。

アンインストール時は ginger.exe を削除してください。

## 使い方

`ginger --help` を参照してください。

```
ginger: HSP3 ビルドツール

使用法:
    ginger <サブコマンド> [オプション]

使用例:
    ginger build hello.hsp

サブコマンド:
    run <スクリプトファイル>
        スクリプトを実行します。(F5 相当)

    build <スクリプトファイル>
        スクリプトを実行形式 (.exe) に変換します。(Ctrl+F9 相当)

グローバルオプション:
    --hsp <HSPのディレクトリ>
        HSP3 のインストールディレクトリへの絶対パスを指定してください。
        省略時は ginger が配置されたディレクトリを使用します。

    --release
        コンパイル時にデバッグ情報を埋め込まず、
        実行時にデバッグウィンドウを表示しません。
        省略時はデバッグモードになります。

    --ref-name <ファイル名>
        コンパイルエラーやデバッグ情報の中で、
        スクリプトのファイル名の代わりに指定された名前を使用します。
        (hsc_refname も参照)

    --obj-name <オブジェクトファイル>
        生成されるオブジェクトファイルの名前を指定します。
        ファイル名は拡張子 .ax を含まなければいけません。
        省略時は start.ax が使用されます。
        (hsc_objname も参照)

    -h, --help
        このヘルプメッセージを表示します。

    -V, --version
        バージョン情報を表示します。
```
