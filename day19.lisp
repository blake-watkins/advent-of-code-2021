(in-package :aoc-2021)

(defun parse-beacon ()
  (with-monad
    (assign coords (parse-number-list))
    (unit coords)))

(defun parse-scanner ()
  (with-monad
    (parse-string "--- scanner ")
    (assign id (parse-number))
    (parse-string " ---")
    (parse-newline)
    (assign beacons (parse-lines (parse-beacon)))
    (unit (list id (fset:convert 'fset:set beacons)))))

(defun parse-file ()
  (parse-list (parse-scanner) (n-of 2 (parse-newline))))

(defun matrix* (a b)
  (when (= (length (first a)) (length b))
    (iter
      (for r below (length a))      
      (collect
          (iter
            (for c below (length (first b)))
            (collect (iter
                       (for i below (length (first a)))
                       (summing (* (elt (elt a r) i)
                                   (elt (elt b i) c))))))))))

(defun matrix-apply (matrix)
  (lambda (point)
    (iter
      (for c in (matrix* matrix (iter (for c in point) (collect (list c)))))
      (collect (first c)))))

(defun transpose-matrix (matrix)
  (iter
    (for r below (length matrix))
    (collect (iter
               (for c below (length (first matrix)))
               (collect (elt (elt matrix c) r))))))

(defparameter *id*
  '((1 0 0)
    (0 1 0)
    (0 0 1)))
(defparameter *r-x*
  '((1 0  0)
    (0 0 -1)
    (0 1  0)))
(defparameter *r-y*
  '(( 0 0 1)
    ( 0 1 0)
    (-1 0 0)))

(defparameter *all-rotations*
  (iter outer
    (with orientation =
          (list *id*
                *r-y*
                (matrix* *r-y* *r-y*)
                (matrix* *r-y* (matrix* *r-y* *r-y*))
                (matrix* *r-x* *r-y* )
                (matrix* *r-x* (matrix* *r-y* (matrix* *r-y* *r-y*)))))
    (for r1 in orientation)
    (iter
      (repeat 4)
      (for r2 first *id* then (matrix* *r-x* r2))
      (in outer (collect (matrix* r1 r2))))))

;; Try to find a rotation and translation that will match MATCHES-REQUIRED
;; beacons in the two point clouds.
;;
;; Given the reference points ref-points and another set of points
;;   - for every possible rotation (24)
;;   - for every pair of possible points in the reference and other set
;;   - find the translation that would match those two points
;;   - translate all of the other set of points and check whether there are
;;     enough matches.
;;   - if there are, return the rotation matrix and the translation vector
(defun match-points (ref-points points matches-required)
  (iter outer
    (for rotation-matrix in *all-rotations*)
    (for rotator = (matrix-apply rotation-matrix))    
    (for rotated-points = (fset:image rotator points))
    (iter
      (for ref-point in-fset ref-points)
      (iter
        (for point in-fset rotated-points)
        (for translation = (point- ref-point point))
        (for translated = (fset:image (lambda (point)
                                        (point+ point translation))
                                      rotated-points))
        (for common-points = (fset:intersection ref-points translated))
        (in outer (finding (list rotation-matrix translation)
                           such-that (>= (fset:size common-points)
                                         matches-required)))))))

;; Given a list of (id beacon-list) lists representing scanners and their
;; beacons, return a map of pairwise transformations to transform between
;; scanner frames. The keys of the returned map are (id1 id2) where the
;; transformation value will map points in scanner id2's frame into scanner
;; id1's frame. 
(defun get-pairwise-transforms (scanners matches-required)
  (let ((uf (make-uf))
        (ret (fset:empty-map))
        (beacons-map (fset:empty-map)))
    (iter
      (for (id beacons) in scanners)
      (fset:includef beacons-map id beacons)
      (uf-make-set id uf))

    (iter
      (for (id-1 . matches) in-fset (prematch-pairs scanners
                                                    matches-required))
      (for beacons-1 = (fset:lookup beacons-map id-1))
      (iter
        (for id-2 in-fset matches)
        (for beacons-2 = (fset:lookup beacons-map id-2))

        (unless (= (uf-find id-1 uf) (uf-find id-2 uf))
          (for match = (match-points beacons-1 beacons-2 matches-required))
          (when match
            (format t "Linked ~a ~a~%" id-1 id-2)
            (uf-union id-1 id-2 uf)
            (fset:includef ret (list id-1 id-2)
                           (cons :normal match))
            (fset:includef ret (list id-2 id-1)
                           (cons :inverted match))))))

    ret))

;; Return a map keyed by scanner id of the transform needed to get closer to
;; the reference frame. The map keys are scanner ids, the values are a list
;; of the new scanner frame and a transform to take points there.
(defun get-reference-transform-map (pairwise-transforms)
  (let ((parents (make-hash-table :test 'equal)))    
    (labels
        ((vertex-fn (vertex parent distance)
           (declare (ignore distance))
           (setf (gethash vertex parents)
                 (cons parent
                       (fset:lookup pairwise-transforms
                                    (list parent vertex)))))
         (neighbour-fn (vertex)
           (iter
             (for pair in-fset pairwise-transforms)
             (when (find vertex pair)
               (collect (list (if (= vertex (first pair))
                                  (second pair)
                                  (first pair))
                              1))))))
      (dijkstra 0 #'vertex-fn #'neighbour-fn)
      parents)))

;; Transform a set of points. A normal transformation applies rotation then
;; adds the translation. An inverted transformation subtracts the translation
;; then applies the inverted (transpose since these are orthonormal matrices)
;; rotation matrix. 
(defun transform-points (points rotation translation &optional (inverted :normal))
  (if (eq inverted :normal)
      (fset:image (lambda (point)
                      (point+ (funcall (matrix-apply rotation) point)
                              translation))
                    points)
      (let ((matrix (transpose-matrix rotation)))        
        (fset:image (lambda (point)
                      (funcall  (matrix-apply matrix)
                                (point- point translation)))
                    points))))

;; Transform set of points to the reference frame from the given frame through
;; the set of transforms in transform-map
(defun transform-to-reference (points frame transform-map)
  (if (= frame 0)
      points
      (destructuring-bind (next-frame inverted rotation translation)
          (gethash frame transform-map)
        (transform-to-reference
         (transform-points points rotation translation inverted)
         next-frame
         transform-map))))

(defun get-coordinate-difference-bags (beacons)
  (iter
    (with ret = (iter
                  (repeat (length (fset:arb beacons)))
                  (collect (fset:empty-bag))))
    (for (a . rest) on (fset:convert 'list beacons))
    
    (iter
      (for b in rest)
      (iter
        (with diff = (point-abs (point- b a)))
        (for i from 0)
        (for d in diff)
        (fset:includef (elt ret i) d)))
    (finally (return ret))))

(defun prematch-pairs (scanners matches-required)
  (let ((diffs-required (floor (* matches-required (1- matches-required)) 2))
        (coord-bags (iter
                      (with ret = (fset:empty-map))
                      (for (id scanner) in scanners)
                      (fset:includef
                       ret id
                       (get-coordinate-difference-bags scanner))
                      (finally (return ret))))
        (ret (fset:empty-map)))
    (iter
      (with initial-set = (fset:convert 'fset:set
                                        (iter
                                          (for (id nil) in scanners)
                                          (collect id))))
      (for (id nil) in scanners)
      (fset:includef ret id initial-set)
      (fset:excludef (fset:lookup ret id) id))
    
    (iter
      (for id in-fset coord-bags)
      (iter
        (for coords in (fset:lookup coord-bags id))
        (iter
          (for other-id in-fset coord-bags)
          (when (/= id other-id)
            (unless (fset:find-if 
                     (lambda (bag)
                       (>= (fset:size (fset:intersection
                                       bag coords))
                           diffs-required))
                     (fset:lookup coord-bags other-id))
              (fset:excludef (fset:lookup ret id) other-id)))))
      (finally (return (fset:sort (fset:convert 'fset:seq ret)
                                  #'<
                                  :key (lambda (x)
                                         (fset:size (cdr x)))))))))

(defun day19 (input &optional (matches-required 12))
  (let* ((parsed (run-parser (parse-file) input))
         (pairwise-transforms
           (get-pairwise-transforms parsed matches-required))
         (transform-map (get-reference-transform-map pairwise-transforms))
         (all-beacons (fset:empty-set))
         (all-scanners (fset:empty-set)))
    (iter
      (for (id beacons) in parsed)
      (fset:unionf all-beacons
                   (transform-to-reference beacons id transform-map))
      (fset:unionf all-scanners
                   (transform-to-reference (fset:set '(0 0 0))
                                           id
                                             transform-map)))
    (list (fset:size all-beacons)
          (iter
            (for (a b) in (pairs (fset:convert 'list all-scanners)))
            (maximizing (manhattan a b))))))
