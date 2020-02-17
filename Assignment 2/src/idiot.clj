(ns idiot
  ;(:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  )

(defn help [arg]
  (cond
    (and (or (= (second arg) "--help") (= (second arg) "-h")) (> (count arg) 1))
    "idiot help: print help for a command\n\nUsage: idiot help <command>\n\nArguments:\n   <command>   the command to print help for\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>\n"
    (and (= (second arg) "init") (> (count arg) 1))
    "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message\n"
    (and (= (second arg) "hash-object") (> (count arg) 1))
    "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file\n"
    (and (= (second arg) "cat-file") (> (count arg) 1))
    "idiot cat-file: print information about an object\n\nUsage: idiot cat-file -p <address>\n\nArguments:\n   -h          print this message\n   -p          pretty-print contents based on object type\n   <address>   the SHA1-based address of the object\n"
    (> (count arg) 2) "Error: invalid command\n"
    :else "idiot help: print help for a command\n\nUsage: idiot help <command>\n\nArguments:\n   <command>   the command to print help for\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>\n")
  )

(defn init [arg]
  (cond
    (or (= (second arg) "--help") (= (second arg) "-h")) (help ["help", "init"])
    (> (count arg) 1) "Error: init accepts no arguments\n"
    :else (if (.isDirectory (io/file ".git")) "Error: .git directory already exists\n"
            (do (io/make-parents ".git/objects/sample.txt")
                "Initialized empty Idiot repository in .git directory\n")))
  )

(defn hash-object [arg]
  (cond
    (and (or (= (second arg) "--help") (= (second arg) "-h")) (> (count arg) 1)) (help ["help", "hash-object"])
    (> (count arg) 1) "Error: invalid command\n"
    :else "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file\n"
    ))

(defn cat-file [arg]
  (cond
    (and (or (= (second arg) "--help") (= (second arg) "-h")) (> (count arg) 1)) (help ["help", "cat-file"])
    (> (count arg) 1) "Error: invalid command\n"
    :else "idiot cat-file: print information about an object\n\nUsage: idiot cat-file -p <address>\n\nArguments:\n   -h          print this message\n   -p          pretty-print contents based on object type\n   <address>   the SHA1-based address of the object\n"))

  (defn -main [& args]
    (cond
      (or (and (= (count args) 1) (= (first args) "help")) (or (or (= (first args) "--help") (= (first args) "-h")) (= (count args) 0)))
      (print "idiot: the other stupid content tracker\n\nUsage: idiot <command> [<args>]\n\nCommands:\n   help\n   init\n   hash-object [-w] <file>\n   cat-file -p <address>\n")
      (= (first args) "help") (print (help args))
      (= (first args) "init") (print (init args))
      (= (first args) "hash-object") (print (hash-object args))
      (= (first args) "cat-file") (print (cat-file args))
      :else (print "Error: invalid command\n")))