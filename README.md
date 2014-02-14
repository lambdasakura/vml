# VML(Visual and Multimedia Library)

VMLはCommon Lispのゲームライブラリです。基本的に2Dゲームでの用途を考えています。


## インストール方法

### lispbuilder-sdlについて

VMLはlispbuilder-sdlが使える環境を想定しています。
lispbuilder-sdlのインストールは面倒なので、[このページ](https://code.google.com/p/lispbuilder/wiki/DownloadInstallationIntro)を参考にインストールしてください。

### 利用しているライブラリ一覧

* lispbuilder-sdl
* lispbuilder-sdl-gfx
* lispbuilder-sdl-image
* lispbuilder-sdl-ttf
* lispbuilder-sdl-mixer
* cl-ppcre
* cl-annot
* cl-test-more
* cl-opengl
* cl-store
* cl-fad
* kmrcl

### VMLのインストール方法

quicklispでインストールできるようにしたいですが、まだ登録依頼も行なっていないので、
以下の様な手動でインストールしてください。

```
cd quicklisp/local-projects/my-evernote
git clone git@github.com:lambdasakura/vml
```

以上で、VMLのインストールは完了です。

## 使い方

これまでの設定で、VMLは利用可能になっているはずです。

SBCLなどの処理系を起動し、

```lisp
(ql:quickload "vml")
```

でVMLがロードされます。

### ウィンドウを表示するだけのサンプル

```lisp
    (defun game-main () )
    (defun game-quit () )
    (defun reload-textures () )
    (defun game-init () )
     
    (defun main ()
      (let ((vml (make-instance 'vml-system:vml-system 
     			    :game-main #'game-main
     			    :game-quit #'game-quit
     			    :tex-reload #'reload-textures
     			    :game-init #'game-init)))
        (vml-system:game-start vml)))
```

## サンプル

いくつか`examples`の下にサンプルコードを置いています。

## リファレンス

まだありません。これから書きます。

## Lisence

Copyright &copy; 2013 lambda_sakura
Distributed under the [MIT License][mit].

[MIT]: http://www.opensource.org/licenses/mit-license.php

