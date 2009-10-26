(defconst jdc-source-extension  ".java"
  "The extension which is used for the generated java files.")

(defconst jdc-object-extension ".class"
  "The extension to look for, if a file is to be decompiled on the fly.")

(defconst jdc-object-extension-pattern "\\.class$"
  "The extension to look for, if a file is to be decompiled on the fly.")

(defgroup jdc nil
  "A Java object file (suffix of '.class') may be
opened up with Emacs, and automatically decompiled into understandable
Java source.  This uses an external decompilation tool, such as Jad."
  :tag "Java Decompilation"
  :group 'jde
  :prefix "jdc-")

(defcustom jdc-command  "jad"
  "The name of the decompiler if it's on your path, otherwise a full
qualified path to it.  The decompilation elisp glue code assumes that
the decompiler tool takes a file name as a last argument."
  :tag "Decompiler command"
  :group 'jdc
  :type 'string)

(defcustom jdc-parameter  '("-space"
                            "-t4"
                            "-lnc"
                            "-b"
                            "-p"
                            "-dead"
                            "-ff"
                            "-i"
                            "-l60"
                            "-nl")
  "Extra parameters which should be added to the call of the
decompiler.  Add one switch per entry.  If you try to put multiple
options into one line, then the options will be passed to the
decompiler as a single option, spaces and all.  If your decompiler
requires a switch to output the Java source text to standared out,
then make sure it is entered here."
  :tag "Command line options"
  :group 'jdc
  :type '(repeat string)
  )

(defcustom jdc-create-source-file-p nil
  "Create a modifible source buffer.  If this is non-nil, then the
buffer is set up as a Java source file that can be saved to file, with
or without modification.  If this is nil, then the buffer is a
read-only source view of the original file.  The read only file can
still be written to another file location."
  :tag "Create source buffer"
  :group 'jdc
  :type 'boolean)

(defun jdc-make-source-name (name-with-class-suffix)
  "If a strings ends with '.class', return it with '.java' as the
suffix.  Otherwise the string is unchanged.  The actual suffix is
controlled by jdc-extenstion"
  (if (string-match jdc-object-extension-pattern name-with-class-suffix)
      (replace-match (concat jdc-source-extension) nil nil name-with-class-suffix)
    name-with-class-suffix))


(defun jdc-buffer ()
  "Construct the command for decompiling a class file, call the resulting
command and load the decompiled file."
  (let ((temp-file-name (concat temporary-file-directory (make-temp-name "jdc") jdc-object-extension))
        (orig-buffer-name (buffer-name))
        (orig-file-name (buffer-file-name)))
    (progn
      (write-file temp-file-name)
      (apply 'call-process-region
             (point-min)
             (point-max)
             jdc-command
             t
             t
             nil
             (append jdc-parameter (list temp-file-name)))
      (if jdc-create-source-file-p
          (progn
            (set-visited-file-name (jdc-make-source-name orig-file-name))
            (let ((new-buffer-name (jdc-make-source-name orig-buffer-name)))
              (condition-case nil
                  (rename-buffer new-buffer-name)
                (error (rename-buffer new-buffer-name t)))))
        (progn
          (set-visited-file-name nil)
          (rename-buffer orig-buffer-name)
          (setq buffer-read-only t)
          (set-buffer-modified-p nil)
          (java-mode)))
      (delete-file temp-file-name))))

;; a hook to be able to automatically decompile-find-file .class files
(add-hook
 'find-file-hooks
 (lambda ()
   (cond ((string-match jdc-object-extension-pattern (buffer-file-name))
          (jdc-buffer)))))

(add-hook 'archive-extract-hooks
          (lambda ()
            (cond ((string-match jdc-object-extension-pattern (buffer-file-name))
                   (jdc-buffer)))))

(provide 'decompile)