(provide 'ajv-dired-rainbow)

;; Read color from a config: https://github.com/Fuco1/dired-hacks/issues/158
;; I am inspired by the approach of vivid. vivid has themes and filetypes
;; https://github.com/sharkdp/vivid
;; https://github.com/sharkdp/vivid/tree/master/themes
;; https://github.com/sharkdp/vivid/blob/master/config/filetypes.yml
;; I've converted one of vivid theme to elisp list with it's filetypes
;; https://gist.github.com/azzamsa/27ed28d7de379173061bf68d8e9798c8
;; https://gist.github.com/azzamsa/5a13a53a6b76a5cb672cc5c793035c39
;;
;; I have a basic way to convert filetypes in vivid yaml into nested alists,
;; Using some python, the code is there in the vivid-yaml-dired-colors directory.


(setq dired-rainbow-list-of-html
      (list "css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
(setq dired-rainbow-list-of-xml
      (list "xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn"
	    "rss" "yaml" "yml" "rdata"))
(setq dired-rainbow-list-of-document
      (list "docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf"
	    "djvu" "epub" "odp" "ppt" "pptx"))
(setq dired-rainbow-list-of-markdown
      (list "org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod"
	    "rst" "tex" "textfile" "txt"))
(setq dired-rainbow-list-of-database
      (list "xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
(setq dired-rainbow-list-of-media
      (list "mp3" "mp4" "m4a" "MP3" "MP4" "avi" "mpeg" "mpg" "flv"
	    "ogg" "mov" "mkv" "mid" "midi" "wav" "aiff" "flac"))
(setq dired-rainbow-list-of-image
      (list "tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
(setq dired-rainbow-list-of-log (list "log"))
(setq dired-rainbow-list-of-shell (list "awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
(setq dired-rainbow-list-of-interpreted
      (list "py" "ipynb" "rb" "spi" "pl" "t" "msql" "mysql" "pgsql" "sql"
	    "r" "clj" "cljs" "scala" "js"))
(setq dired-rainbow-list-of-compiled
      (list "asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m"
	    "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08"
	    "s" "rs" "hi" "hs" "pyc" ".java"))
(setq dired-rainbow-list-of-executable (list "exe" "msi"))
(setq dired-rainbow-list-of-compressed
      (list "7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear"
	    "rar" "sar" "xpi" "apk" "xz" "tar"))
(setq dired-rainbow-list-of-packaged
      (list "deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
(setq dired-rainbow-list-of-encrypted
      (list "gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
(setq dired-rainbow-list-of-fonts (list "afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
(setq dired-rainbow-list-of-partition
      (list "dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
(setq dired-rainbow-list-of-vc (list "git" "gitignore" "gitattributes" "gitmodules"))

(dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
(dired-rainbow-define html "#eb5286" dired-rainbow-list-of-html)
(dired-rainbow-define xml "#f2d024" dired-rainbow-list-of-xml)
(dired-rainbow-define document "#9561e2" dired-rainbow-list-of-document)
(dired-rainbow-define markdown "#ffed4a" dired-rainbow-list-of-markdown)
(dired-rainbow-define database "#6574cd" dired-rainbow-list-of-database)
(dired-rainbow-define media "#de751f" dired-rainbow-list-of-media)
(dired-rainbow-define image "#f66d9b" dired-rainbow-list-of-image)
(dired-rainbow-define log "#c17d11" dired-rainbow-list-of-log)
(dired-rainbow-define shell "#f6993f" dired-rainbow-list-of-shell)
(dired-rainbow-define interpreted "#38c172" dired-rainbow-list-of-interpreted)
(dired-rainbow-define compiled "#4dc0b5" dired-rainbow-list-of-compiled)
(dired-rainbow-define executable "#8cc4ff" dired-rainbow-list-of-executable)
(dired-rainbow-define compressed "#51d88a" dired-rainbow-list-of-compressed)
(dired-rainbow-define packaged "#faad63" dired-rainbow-list-of-packaged)
(dired-rainbow-define encrypted "#ffed4a" dired-rainbow-list-of-encrypted)
(dired-rainbow-define fonts "#6cb2eb" dired-rainbow-list-of-fonts)
(dired-rainbow-define partition "#e3342f" dired-rainbow-list-of-partition)
(dired-rainbow-define vc "#0074d9" dired-rainbow-list-of-vc)
(dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")

;; (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
;; (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
;; (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
;; (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
;; (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
;; (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
;; (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mkv" "mid" "midi" "wav" "aiff" "flac"))
;; (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
;; (dired-rainbow-define log "#c17d11" ("log"))
;; (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
;; (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
;; (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
;; (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
;; (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
;; (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
;; (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
;; (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
;; (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
;; (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
;; (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
