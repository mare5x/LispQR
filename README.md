# LispQR

QR Code encoding written in **Common Lisp**.


## Usage

To write to PNG files, you will need to install [_zpng_](https://www.xach.com/lisp/zpng/). If you load LispQR using _lispqr.asd_, all dependencies will get installed automatically:
```lisp 
(ql:quickload "lispqr")
```

```lisp
;; Compile the files (lispqr.asd):
;; (ql:quickload "lispqr")

;; Change into the 'encode' package.
(in-package :mare5x.lispqr.encode)  ;; (in-package :lispqr)

;; Encode to a PNG image file.
(encode->image "https://github.com/mare5x" "test.png" :ec-level :H)
```
  
![https://github.com/mare5x](./mare5x-github.png)   
(string: "https://github.com/mare5x", version: 4, error correction level: high, encoding mode: byte)  


### Portacle

Checklist to get up and running using Portacle.
  1. Install [Portacle](https://portacle.github.io/).
  2. _Ctrl-X, Ctrl-F_ \<path to repo/lispqr.asd\>
  3. Focus the code buffer and press _Ctrl-C, Ctrl-K_ to load the current file.
  4. ```(ql:quickload "lispqr")``` in the REPL.
  5. Now you are ready to tinker with LispQR.

## References

Written based on QR standards "ISO/IEC 18004:2000" and "ISO/IEC 18004:2006".

Great additional resources:
  * https://www.thonky.com/qr-code-tutorial/
  * https://www.nayuki.io/page/creating-a-qr-code-step-by-step

