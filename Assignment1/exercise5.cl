(defun bfs (end queue net)
  (if (null queue) 
      '()
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end)
          (reverse path)
        (bfs end
             (append (rest queue)
                     (new-paths path node net))
             net)))))


(defun new-paths (path node net)
  (mapcar #'(lambda (n)(cons n path))
    (rest (assoc node net))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

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

(defun shortest-path-improved (start end net)
  (bfs-improved end (list (list start)) net NIL))