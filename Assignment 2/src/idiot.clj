(ns idiot
  ;(:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:import (java.io File))
  (:import java.security.MessageDigest)
  (:import java.util.zip.DeflaterOutputStream
           (java.io ByteArrayInputStream
                    ByteArrayOutputStream)
           java.util.zip.InflaterInputStream))


(defn help [arg]
  (cond
    (and (or (= (second arg) "--help") (= (second arg) "help") (= (second arg) "-h")) (> (count arg) 1))
    "idiot help: print help for a command\n\nUsage: idiot help <command>\n\nArguments:\n   <command>   the command to print help for\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>\n"
    (and (= (second arg) "init") (> (count arg) 1))
    "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message\n"
    (and (= (second arg) "hash-object") (> (count arg) 1))
    "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file"
    (and (= (second arg) "cat-file") (> (count arg) 1))
    "idiot cat-file: print information about an object\n\nUsage: idiot cat-file -p <address>\n\nArguments:\n   -h          print this message\n   -p          pretty-print contents based on object type\n   <address>   the SHA1-based address of the object\n"
    (> (count arg) 2) "Error: invalid command\n"
    :else "Error: invalid command\n")
  )

(defn init [arg]
  (cond
    (or (= (second arg) "--help") (= (second arg) "-h")) (help ["help", "init"])
    (> (count arg) 1) "Error: init accepts no arguments\n"
    :else (if (.isDirectory (io/file ".git")) "Error: .git directory already exists\n"
                                              (do (io/make-parents ".git/objects/sample.txt")
                                                  "Initialized empty Idiot repository in .git directory\n")))
  )

(defn sha1-hash-bytes [data]
  (.digest (MessageDigest/getInstance "sha1")
           (.getBytes data)))

(defn byte->hex-digits [byte]
  (format "%02x"
          (bit-and 0xff byte)))

(defn bytes->hex-string [bytes]
  (->> bytes
       (map byte->hex-digits)
       (apply str)))

(defn sha1-sum [header+blob]
  (bytes->hex-string (sha1-hash-bytes header+blob)))

(defn zip-str
  "Zip the given data with zlib. Return a ByteArrayInputStream of the zipped
  content."
  [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))

(defn unzip
  "Unzip the given data with zlib. Pass an opened input stream as the arg. The
  caller should close the stream afterwards."
  [input-stream]
  (with-open [unzipper (InflaterInputStream. input-stream)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (->> (.toByteArray out)
         (map char)
         (apply str))))

(defn hash-object-helper [switch, file-name]
  (let [header-and-blob (str "blob " (count (slurp file-name)) "\000" (slurp file-name))
        address (sha1-sum header-and-blob)
        directory (subs address 0 2)
        file-name (subs address 2)]
    (if (= "-w" switch)
      (do
        (.mkdir (File. (str ".git/objects/" directory)))
        (io/copy (zip-str header-and-blob) (io/file (str ".git/objects/" directory "/" file-name)))
        address)
      address)))

(defn hash-object [arg]
  (cond
    (and (= 1 (count arg)) (or (= "-h" (first arg)) (= "--help" (first arg)))) (help ["help" "hash-object"])
    (not (.isDirectory (io/file ".git"))) "Error: could not find database. (Did you run `idiot init`?)"
    (and (= 2 (count arg)) (= "-w" (first arg))) (if (.isFile (io/file (second arg)))
                                                   (hash-object-helper "-w" (second arg))
                                                   "Error: that file isn't readable")
    (and (= 1 (count arg)) (not (= "-w" (first arg)))) (if (.isFile (io/file (first arg)))
                                                         (hash-object-helper "" (first arg))
                                                         "Error: that file isn't readable")
    :else "Error: you must specify a file."))

(defn cat-file [arg]
  (cond
    (and (= 1 (count arg)) (or (= "-h" (first arg)) (= "--help" (first arg)))) (help ["help" "cat-file"])
    (not (.isDirectory (io/file ".git"))) "Error: could not find database. (Did you run `idiot init`?)\n"
    (and (= 2 (count arg)) (= "-p" (first arg))) (let [blob-location (str ".git/objects/" (subs (second arg) 0 2) "/" (subs (second arg) 2))]
                                                   (if (.isFile (io/file blob-location))
                                                     (let [unzipped-blob (with-open [input (-> blob-location io/file io/input-stream)] (unzip input))
                                                           start-of-blob (.indexOf unzipped-blob "\000")]
                                                       (subs unzipped-blob (+ start-of-blob 1)))
                                                     "Error: that address doesn't exist\n"))
    (< 0 (count arg)) "Error: you must specify an address\n"
    :else "Error: the -p switch is required\n"))

;(defn cat-file [arg]
;  (cond
;    (and (or (= (second arg) "--help") (= (second arg) "-h")) (> (count arg) 1)) (help ["help", "cat-file"])
;    (not (.isDirectory (io/file ".git"))) "Error: could not find database. (Did you run `idiot init`?)\n"
;    (and (= "-p" (second arg)) (> 1 (count arg))) (let [blob-location (str ".git/objects/" (subs (nth arg 2) 0 2) "/" (subs (nth arg 2) 2))]
;                                                   (if (.isFile (io/file blob-location))
;                                                     (let [unzipped-blob (with-open [input (-> blob-location io/file io/input-stream)] (unzip input))
;                                                           start-of-blob (.indexOf unzipped-blob "\000")]
;                                                       (println (subs unzipped-blob (+ start-of-blob 1))))
;                                                     (println "Error: that address doesn't exist")))
;    (< (count arg) 0) "Error: you must specify an address\n"
;    :else "Error: the -p switch is required\n"))


(defn -main [& args]
  (cond
    (or (and (= (count args) 1) (= (first args) "help")) (or (or (= (first args) "--help") (= (first args) "-h")) (= (count args) 0)))
    (print "idiot: the other stupid content tracker\n\nUsage: idiot <command> [<args>]\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>\n")
    (= (first args) "help") (print (help args))
    (= (first args) "init") (print (init args))
    (= (first args) "hash-object") (println (hash-object (drop 1 args)))
    (= (first args) "cat-file") (print (cat-file (drop 1 args)))
    :else (print "Error: invalid command\n")))