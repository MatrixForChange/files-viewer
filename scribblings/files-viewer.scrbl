#lang scribble/manual

@(require racket/file
          racket/path
          racket/runtime-path
          (for-syntax racket/base) ; for build-path in runtime-path
          (for-label racket/gui)
          (for-label drracket/tool-lib))

@(define (codeblock/file file)
   (list
    @(filebox (path->string (file-name-from-path file)) "")
    @(codeblock (file->string file))))

@title{Files-viewer: a file manager plugin for DrRacket}

@;author{风变科技-矩阵工作室}
@(smaller (author+email "风变科技-矩阵工作室 Qi Jiang" "1227847322@qq.com" #:obfuscate? #t))

@section{Introduction}

Files-viewer is a file manager plugin for DrRacket.

@section{Installation}

To install, either look for @tt{files-manager} in the DrRacket menu @italic{File>Package Manager},
or run the raco command:
@commandline{raco pkg install files-manager}

You need to restart DrRacket. Now you should have a new item @italic{Show the File Manager} in the @italic{View} menu.

@section{Usage}

Click @italic{View} then @italic{Show the File Manager}.

To edit a file, double click the item.

To call the popup menu, right click the file manager.

To show or hide the file manager, View -> Show/Hide the File Manager.

