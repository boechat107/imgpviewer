(ns image-processing.file-utils
  (:import (java.io File)))

(defn File? [file]
  "Test if object is of type java.io.File"
  #^{:arglist [file]}
  (= (class file) java.io.File))

(defn file-name
  "Given a File, returns its name stripping the extension"
  #^{:arglist [file]}
  ([file]
      {:pre [(File? file)]}
      (let [filename (.getName file)
            ext-pos (.lastIndexOf filename ".")]
        (if (> ext-pos 0)
          (subs filename 0 ext-pos)
          filename))))

(defn file-ext
  "Given a File, returns its extension (by default 4 chars from
  right to left."
  ;; TODO: use split function to split the string into two parts: the file name and the
  ;; extension.
  #^{:arglist [[file] [file n]]}
  ([file]
      {:pre [(File? file)]}
      (file-ext file 4))

  ([file n]
      {:pre [(File? file)]}
      (let [filename (.getName file)]
        (if (-> filename count (> n))
          (subs filename (- (count filename) n) (count filename))))))

(defn file-img? [file]
  "Test if file is an image:
      -isFile?
      -extension (jpg png bmp gif)"
  #^{:arglist [file]}
  (let [img-exts #{".jpg" ".png" ".bmp" ".gif"}]
    (if (.isFile file)
      (contains? img-exts (file-ext file)))))
