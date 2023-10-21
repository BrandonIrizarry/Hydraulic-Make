;; -*- lexical-binding: t; -*-

;;; Simple definitions involving the locations of important files and
;;; directories.

(defvar *java-project-root*
  "~/eclipse-workspace2/UCSDGraphs/"
  "The top of the UCSDGraphs project hierarchy. Note that the actual
packages are under 'src'.")

(defvar *java-project-package-root*
  (concat *java-project-root* "src/")
  "The top of the UCSDGraphs package hierarchy.")

;;; RX definitions to help us strip comments from our working temp
;;; buffer.

(rx-define java-multi-line-comment
    (: "/*" (*
             (| (not "*")
                (: "*" (not "/"))))
     (+ "*") "/"))

(rx-define java-line-comment
    (: "//" (* not-newline) eol))

(rx-define java-identifier
    (: (any alpha "_") (* (any alnum "_"))))

;;; The "package-table" object: make it easy to look up a list of
;;; files, given a package name.

(cl-defstruct (package-table (:constructor package-table--create))
  "A hash table mapping a package to the files it encompasses."
  table)

(defun package-table-create ()
  (package-table--create :table (generate-package-table)))

(cl-defmethod get-file-list ((this package-table) package-name)
  (gethash package-name (package-table-table this)))

(cl-defmethod get-packages ((this package-table))
  (hash-table-keys (package-table-table this)))

(cl-defmethod to-assoc-list ((this package-table))
  (let (result)
    (maphash (lambda (k v) (push (cons k (list v)) result)) (package-table-table this))
    result))

(cl-defmethod lookup-file ((this package-table) package-unit)
  "Given PACKAGE-UNIT (e.g. 'application.MapApp'), return the
corresponding file."
  (let* ((package-name (extract-package-name-from-unit package-unit))
         (files (get-file-list this package-name))
         (with-slashes (replace-regexp-in-string (rx ".") "/" package-unit)))
    (cl-find (format "%s.java" with-slashes) files :test #'string=)))

(cl-defmethod find-dependencies ((this package-table) full-filename)
  (let* ((lines (get-program-lines full-filename))
         ;; For inline package references. We're using a hash table
         ;; here as a hash set, to avoid duplicates.
         (mentions (make-hash-table :test #'equal))
         ;; Here, we explicitly assume that the directory hierarchy
         ;; and the package hierarchy are one and the same thing.
         (local-files (thread-last
                        full-filename
                        (find-package-name)
                        (get-file-list this)
                        (remove (find-simple-filename full-filename)))))
    (dolist (line lines)
      (pcase line
        ;; Skip the 'package' declaration, since these contain
        ;; "dotted" statements which our default case would otherwise
        ;; catch.
        ((rx bos (* space) "package") nil)
        ((rx bos (* space) "import")
         (let* ((words (reverse (string-split line (rx (any " ;")) t)))
                (package-unit (car words))
                (package-name (extract-package-name-from-unit package-unit)))
           (if (string-match-p (rx "*" eos) package-unit)
               (let ((file-list (get-file-list this package-name)))
                 (dolist (file file-list)
                   (puthash file t mentions)))
             (let ((file (lookup-file this package-unit)))
               ;; Avoid 'nil' (for example, when dealing with
               ;; something like 'java.util')
               (when (stringp file)
                 (puthash file t mentions))))))
        (_

         ;; Find local dependencies
         (let ((live-local-files
                (cl-remove-if-not (lambda (local-file)
                                    (let ((local (file-name-base local-file)))
                                      (when (string-match (rx-to-string `(seq (not (any alpha "_")) (group ,local) (not (any alnum "_"))))
                                                          line)
                                        (match-string-no-properties 1 line))))
                                  local-files)))
           (dolist (local-file live-local-files)
             (puthash local-file t mentions)))

         ;; Find inline dependencies
         (dolist (package-name (get-packages this))
           (if (string-match (rx-to-string `(seq ,package-name "." java-identifier)) line)
               (let ((package-unit (match-string-no-properties 0 line)))
                 (puthash (lookup-file this package-unit) t mentions)))))))
    (hash-table-keys mentions)))

(cl-defmethod generate-dependency-graph ((this package-table))
  (let ((all-files (get-all-files))
        (graph (make-hash-table :test #'equal)))
    (dolist (file all-files graph)
      (puthash file (find-dependencies this file) graph))))

(cl-defstruct (dependency-graph (:constructor dependency-graph--create))
              graph)

(defun dependency-graph-create ()
  (dependency-graph--create :graph (generate-dependency-graph (package-table-create))))

(cl-defmethod to-assoc-list ((this dependency-graph))
  (let (result)
    (maphash (lambda (k v) (push (cons k (list v)) result)) (dependency-graph-graph this))
    result))

(cl-defmethod get-dependencies ((this dependency-graph) full-filename)
  (let ((table (dependency-graph-graph this)))
    (gethash full-filename table)))

;;; Some helper functions

(defun find-package-name (full-filename)
  "Return the package FULL-FILENAME belongs to, as a string."
  (with-temp-buffer
    (insert-file full-filename)
    (goto-char (point-min))
    (re-search-forward (rx "package" (+ space) (group (+ not-newline)) ";"))
    (match-string-no-properties 1)))

(defun find-simple-filename (full-filename)
  "Return the relative-path equivalent of the package that
FULL-FILENAME belongs to."
  (replace-regexp-in-string (rx-to-string `(seq bos ,*java-project-package-root*)) "" full-filename))

(defun get-all-files ()
  (directory-files-recursively *java-project-package-root*
                               (rx bol (not (any ".#")) (* not-newline) ".java" eol)))

(defun generate-package-table ()
  "Return a hash table mapping a package to the list of files
it encompasses."
  (let ((known-packages (make-hash-table :test #'equal))
        (all-files (get-all-files)))
    (dolist (file all-files known-packages)
      (let* ((basic-name
              (progn
                ;; Extract into BASIC-NAME the path that would
                ;; correspond to the fully qualified package name.
                (string-match (rx-to-string `(seq bol ,*java-project-package-root* (group (+ not-newline)) eol))
                              file)
                (match-string-no-properties 1 file)))
             ;; This "opens" the file to discover what the package
             ;; declaration looks like inside it.
             (package-name (find-package-name file))
             (file-list (gethash package-name
                                 known-packages
                                 ;; The default if PACKAGE-NAME
                                 ;; hasn't been added yet
                                 (list))))
        (push basic-name file-list)
        (puthash package-name file-list known-packages)))))

(defun get-program-lines (full-filename)
  "Return the (non-empty) lines of FULL-FILENAME, after first having
stripped away comments."
  (with-temp-buffer
    (insert-file full-filename)
    (let ((content (thread-last
                     (buffer-string)
                     (replace-regexp-in-string (rx java-multi-line-comment) "")
                     (replace-regexp-in-string (rx java-line-comment) ""))))
      (thread-last
        (string-split content "\n" t)
        (mapcar #'string-trim)
        (seq-filter (lambda (line) (not (string-empty-p line))))))))

;;; Tests.

(defun extract-package-name-from-unit (package-unit)
  (mapconcat #'identity (butlast (string-split package-unit (rx ".") t)) "."))

(require 'ert)

(ert-deftest fetch-package-files-for-roadgraph ()
  (let ((files (mapcar (lambda (name) (format "roadgraph/%s.java" name))
                       '("AStarGrader" "CorrectAnswer" "DijkstraGrader" "MapGraph" "SearchGrader")))
        (package-table (generate-package-table)))
    (should (equal (sort (gethash "roadgraph" package-table) #'string<)
                   (sort files #'string<)))))

(ert-deftest mapapp-in-package-application ()
  (should (equal (find-package-name (concat *java-project-package-root* "application/MapApp.java"))
                 "application")))

(ert-deftest check-file-list ()
  (let* ((table-obj (package-table-create))
         (file-list (cddr (directory-files (concat *java-project-package-root* "gmapsfx/javascript/event"))))
         (computed-file-list (get-file-list table-obj "gmapsfx.javascript.event")))
    (should (equal (sort (mapcar #'file-name-base file-list)
                         #'string<)
                   (sort (mapcar #'file-name-base computed-file-list)
                         #'string<)))))

(ert-deftest test-number-of-packages-is-17 ()
  (let ((table-obj (package-table-create)))
    (should (eql 17 (length (get-packages table-obj))))))

(ert-deftest test-program-lines-small-example ()
  (should (equal (get-program-lines "~/eclipse-workspace2/UCSDGraphs/src/gmapsfx/javascript/event/UIEventType.java")
                 '("package gmapsfx.javascript.event;" "public enum UIEventType {" "click, dblclick, mousemove, mouseup, mousedown, mouseover, mouseout, rightclick;" "}"))))

;; This one was simply lifted from a IELM session, and so may be
;; spurious, but at least it demonstrates what our program intends to
;; do.
(ert-deftest test-program-lines-large-example ()
  (let ((program-lines (get-program-lines "~/eclipse-workspace2/UCSDGraphs/src/basicgraph/GraphAdjList.java"))
        (large-example '("package basicgraph;" "import java.util.ArrayList;" "import java.util.HashMap;" "import java.util.List;" "import java.util.Map;" "import java.util.Set;" "public class GraphAdjList extends Graph {" "private Map<Integer, ArrayList<Integer>> adjListsMap;" "public GraphAdjList () {" "adjListsMap = new HashMap<Integer, ArrayList<Integer>>();" "}" "public void implementAddVertex() {" "int nextIndex = getNumVertices();" "ArrayList<Integer> neighbors = new ArrayList<Integer>();" "adjListsMap.put(nextIndex,  neighbors);" "}" "public void implementAddEdge(int v, int w) {" "int numVertices = getNumVertices();" "if (v < 0 || w < 0 || v >= numVertices || w >= numVertices) {" "throw new IndexOutOfBoundsException();" "}" "ArrayList<Integer> neighbors = adjListsMap.get(v);" "if (neighbors == null) {" "neighbors = new ArrayList<Integer>();" "adjListsMap.put(v, neighbors);" "}" "neighbors.add(w);" "}" "public List<Integer> getNeighbors(int v) {" "return new ArrayList<Integer>(adjListsMap.get(v));" "}" "public List<Integer> getInNeighbors(int v) {" "List<Integer> inNeighbors = new ArrayList<Integer>();" "for (int u : adjListsMap.keySet()) {" "for (int w : adjListsMap.get(u)) {" "if (v == w) {" "inNeighbors.add(u);" "}" "}" "}" "return inNeighbors;" "}" "public List<Integer> getDistance2(int v) {" "List<Integer> reachable2 = new ArrayList<>();" "List<Integer> neighbors = getNeighbors(v);" "for (int i : neighbors) {" "reachable2.addAll(getNeighbors(i));" "}" "return reachable2;" "}" "public String adjacencyString() {" "String s = \"Adjacency list\";" "s += \" (size \" + getNumVertices() + \"+\" + getNumEdges() + \" integers):\";" "for (int v : adjListsMap.keySet()) {" "s += \"\\n\\t\"+v+\": \";" "for (int w : adjListsMap.get(v)) {" "s += w+\", \";" "}" "}" "return s;" "}" "}")))
    (should (equal program-lines
                   large-example))
    ;; Verify that there are no empty strings in the set of lines.
    (should (cl-every (lambda (line) (not (string-empty-p line))) program-lines))))

(ert-deftest test-dependencies-basic ()
  "Test both 'import', and inlined dependencies.

In this particular example, the first entry is an inline
dependency; the rest are grabbed via 'import'."
  (should (equal (find-dependencies (package-table-create) (concat *java-project-package-root* "application/MapApp.java"))
                 '("geography/GeographicPoint.java" "gmapsfx/javascript/object/MapTypeIdEnum.java" "gmapsfx/javascript/object/MapOptions.java" "gmapsfx/javascript/object/LatLong.java" "gmapsfx/javascript/object/GoogleMap.java" "gmapsfx/MapComponentInitializedListener.java" "gmapsfx/GoogleMapView.java" "application/services/RouteService.java" "application/services/GeneralService.java" "application/controllers/RouteController.java" "application/controllers/FetchController.java")))

  (should (equal (find-dependencies (package-table-create) (concat *java-project-package-root* "application/services/GeneralService.java"))
                 '("mapmaker/MapMaker.java" "gmapsfx/javascript/object/LatLongBounds.java" "gmapsfx/javascript/object/LatLong.java" "gmapsfx/javascript/object/GoogleMap.java" "gmapsfx/GoogleMapView.java" "application/SelectManager.java" "application/MarkerManager.java" "application/MapApp.java" "application/DataSet.java"))))
