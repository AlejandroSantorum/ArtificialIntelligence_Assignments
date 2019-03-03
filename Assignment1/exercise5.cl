;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;   PROJECT: Assignment 1 - Artificial Intelligence                          ;;
;;   FILE: exercise5.cl                                                       ;;
;;   AUTHORS:                                                                 ;;
;;     · Alejandro Santorum Varela - alejandro.santorum@estudiante.uam.es     ;;
;;     · Sergio Galan Martin - sergio.galanm@estudiante.uam.es                ;;
;;   DATE: February 17, 2019                                                  ;;
;;   VERSION: 1.2                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;      exercise 5      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bfs
;; Explores the graph represented by net looking for the node end
;;
;; INPUT: end: final state
;; queue: list of starting nodes
;; net: list of lists representing a graph
;; OUTPUT: shortest path from any of the starting nodes to end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bfs (end queue net)
  (if (null queue) ;; return error (si lista vacia)
      '()
    (let* ((path (first queue)) 
           (node (first path))) ;; m = C.pop()
      (if (eql node end) ;; if node is destination (end)
          (reverse path) ;; return path
        (bfs end ;; recursive call is analogous to the for loop
             (append (rest queue) ;; C.enqueue node
                     (new-paths path node net)) ;; generating new posible paths
             net)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new-paths
;; Gets the neighbours of a given node
;;
;; INPUT: path: already explored nodes
;; node: node to explore
;; net: list of lists representing a graph
;; OUTPUT: list of lists with all the posible dispositions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun new-paths (path node net)
  (mapcar #'(lambda (n)(cons n path))
    (rest (assoc node net))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shortest-path
;; Gets the shortest path from start to end
;;
;; INPUT: start: begining node
;; end: final node
;; net: list of lists representing a graph
;; OUTPUT: shortest path from start to end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bfs-improved
;; Explores the graph represented by net looking for the node end
;; This function avoids infinite loop when looking for a nonexistent path and
;; there are loops in the graph.
;;
;; INPUT: end: final state
;; queue: list of starting nodes
;; net: list of lists representing a graph
;; OUTPUT: shortest path from any of the starting nodes to end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bfs-improved (end queue net checked)
  (if (null queue)
      '()
    (let* ((path (first queue))
           (node (first path)))
      (cond
       ((eql node end)
        (reverse path))
       ((contains node checked)
        (bfs-improved end (rest queue) net checked))
       (T (bfs-improved end
             (append (rest queue)
                     (new-paths path node net))
                        net (append checked (cons node NIL))))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shortest-path
;; Gets the shortest path from start to end. This function avoids infinite
;; loop when looking for a nonexistent path and there are loops in the graph.
;;
;; INPUT: start: begining node
;; end: final node
;; net: list of lists representing a graph
;; OUTPUT: shortest path from start to end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shortest-path-improved (start end net)
  (bfs-improved end (list (list start)) net NIL))