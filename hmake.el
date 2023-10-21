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

;;; The program itself ensues here.

(defun find-package-name (full-filename)
  "Return the package FULL-FILENAME belongs to, as a string."
  (with-temp-buffer
    (insert-file full-filename)
    (goto-char (point-min))
    (re-search-forward (rx "package" (+ space) (group (+ not-newline)) ";"))
    (match-string-no-properties 1)))

(defun generate-package-table ()
  "Return a hash table mapping a package to the list of files
it encompasses."
  (let ((known-packages (make-hash-table :test #'equal))
        (all-files (directory-files-recursively *java-project-package-root*
                                                (rx bol (not (any ".#")) (* not-newline) ".java" eol))))
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
