# image-processing

This is a library for raw image processing in Clojure.

## Some features

* Loads images using the ImageIO Java library.
* Image visualization.
* Basic transformations:
 - change color space
 - erode
 - binarization

## Some implementation details

The basic data structure used in this library is defined as record called `Image`, whose fields are `:mat` and `:type`. The value of `:mat` is always assumed to be a vector of vectors, like a matrix, that represents each pixel of an image. If an `Image` object has `:type` value equals to `:rgb` (the default color space), each pixel is represented by vector `[r g b]`. If `:type` is `:gray` (grayscale), each pixel is represented just by a number, the intensity of the pixel (usually in the interval `[0, 255]`). 

## Basic usage 

```clj
(ns image-processing.test.processing 
  (:require 
    [image-processing.core-new :as ipc]
    [image-processing.processing :as pr]
    [image-processing.helpers :as ih]))

(let [img (ih/load-file-image "test/test.jpg"),
        gray (pr/rgb-to-gray img),
        bw (pr/binarize gray 100),
        er (pr/erode bw)]
    (ih/view img gray bw er))
```

## Installation

Add to the dependencies of a leiningen project:

```clj
[org.clojars.boechat107/image-processing "2.0.0-SNAPSHOT"]
```
