(set-logic HORN)
(declare-fun
   INV_REC_f
   (Int
    Int
    Int
    Int)
   Bool)
(declare-fun
   INV_REC_f_PRE
   (Int
    Int)
   Bool)
(declare-fun
   INV_REC_f__1
   (Int
    Int)
   Bool)
(declare-fun
   INV_REC_f__1_PRE
   (Int)
   Bool)
(declare-fun
   INV_REC_f__2
   (Int
    Int)
   Bool)
(declare-fun
   INV_REC_f__2_PRE
   (Int)
   Bool)
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (and
            (= n$1_0_old n$2_0_old))
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               _$1_0
               (let
                  ((r.0$1_0 n$1_0_old))
                  (let
                     ((result$1 r.0$1_0)
                      (_$2_0 (<= n$2_0_old 1)))
                     (=>
                        _$2_0
                        (let
                           ((r.0$2_0 n$2_0_old))
                           (let
                              ((result$2 r.0$2_0))
                              (= result$1 result$2)))))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (and
            (= n$1_0_old n$2_0_old))
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               _$1_0
               (let
                  ((r.0$1_0 n$1_0_old))
                  (let
                     ((result$1 r.0$1_0)
                      (_$2_0 (<= n$2_0_old 1)))
                     (=>
                        (not _$2_0)
                        (let
                           ((_$2_1 (- n$2_0_old 2)))
                           (and
                              (INV_REC_f__2_PRE _$2_1)
                              (forall
                                 ((_$2_2 Int))
                                 (=>
                                    (INV_REC_f__2 _$2_1 _$2_2)
                                    (let
                                       ((_$2_1 (- n$2_0_old 2))
                                        (_$2_3 (- n$2_0_old 1)))
                                       (let
                                          ((_$2_4 (+ n$2_0_old _$2_3)))
                                          (let
                                             ((_$2_5 (+ _$2_4 _$2_2)))
                                             (let
                                                ((r.0$2_0 _$2_5))
                                                (let
                                                   ((result$2 r.0$2_0))
                                                   (= result$1 result$2))))))))))))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (and
            (= n$1_0_old n$2_0_old))
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((_$1_1 (- n$1_0_old 1)))
                  (let
                     ((_$1_2 (<= _$1_1 1)))
                     (=>
                        _$1_2
                        (let
                           ((rx.0$1_0 _$1_1))
                           (let
                              ((_$1_6 (+ n$1_0_old rx.0$1_0)))
                              (let
                                 ((r.0$1_0 _$1_6))
                                 (let
                                    ((result$1 r.0$1_0)
                                     (_$2_0 (<= n$2_0_old 1)))
                                    (=>
                                       _$2_0
                                       (let
                                          ((r.0$2_0 n$2_0_old))
                                          (let
                                             ((result$2 r.0$2_0))
                                             (= result$1 result$2))))))))))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (and
            (= n$1_0_old n$2_0_old))
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((_$1_1 (- n$1_0_old 1)))
                  (let
                     ((_$1_2 (<= _$1_1 1)))
                     (=>
                        _$1_2
                        (let
                           ((rx.0$1_0 _$1_1))
                           (let
                              ((_$1_6 (+ n$1_0_old rx.0$1_0)))
                              (let
                                 ((r.0$1_0 _$1_6))
                                 (let
                                    ((result$1 r.0$1_0)
                                     (_$2_0 (<= n$2_0_old 1)))
                                    (=>
                                       (not _$2_0)
                                       (let
                                          ((_$2_1 (- n$2_0_old 2)))
                                          (and
                                             (INV_REC_f__2_PRE _$2_1)
                                             (forall
                                                ((_$2_2 Int))
                                                (=>
                                                   (INV_REC_f__2 _$2_1 _$2_2)
                                                   (let
                                                      ((_$2_1 (- n$2_0_old 2))
                                                       (_$2_3 (- n$2_0_old 1)))
                                                      (let
                                                         ((_$2_4 (+ n$2_0_old _$2_3)))
                                                         (let
                                                            ((_$2_5 (+ _$2_4 _$2_2)))
                                                            (let
                                                               ((r.0$2_0 _$2_5))
                                                               (let
                                                                  ((result$2 r.0$2_0))
                                                                  (= result$1 result$2)))))))))))))))))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (and
            (= n$1_0_old n$2_0_old))
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((_$1_1 (- n$1_0_old 1)))
                  (let
                     ((_$1_2 (<= _$1_1 1)))
                     (=>
                        (not _$1_2)
                        (let
                           ((_$1_3 (- _$1_1 1))
                            (_$2_0 (<= n$2_0_old 1)))
                           (=>
                              _$2_0
                              (let
                                 ((r.0$2_0 n$2_0_old))
                                 (let
                                    ((result$2 r.0$2_0))
                                    (and
                                       (INV_REC_f__1_PRE _$1_3)
                                       (forall
                                          ((_$1_4 Int))
                                          (=>
                                             (INV_REC_f__1 _$1_3 _$1_4)
                                             (let
                                                ((_$1_3 (- _$1_1 1))
                                                 (_$1_5 (+ _$1_1 _$1_4)))
                                                (let
                                                   ((rx.0$1_0 _$1_5))
                                                   (let
                                                      ((_$1_6 (+ n$1_0_old rx.0$1_0)))
                                                      (let
                                                         ((r.0$1_0 _$1_6))
                                                         (let
                                                            ((result$1 r.0$1_0))
                                                            (= result$1 result$2)))))))))))))))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (and
            (= n$1_0_old n$2_0_old))
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((_$1_1 (- n$1_0_old 1)))
                  (let
                     ((_$1_2 (<= _$1_1 1)))
                     (=>
                        (not _$1_2)
                        (let
                           ((_$1_3 (- _$1_1 1))
                            (_$2_0 (<= n$2_0_old 1)))
                           (=>
                              (not _$2_0)
                              (let
                                 ((_$2_1 (- n$2_0_old 2)))
                                 (and
                                    (INV_REC_f_PRE _$1_3 _$2_1)
                                    (forall
                                       ((_$1_4 Int)
                                        (_$2_2 Int))
                                       (=>
                                          (INV_REC_f _$1_3 _$2_1 _$1_4 _$2_2)
                                          (let
                                             ((_$1_3 (- _$1_1 1))
                                              (_$1_5 (+ _$1_1 _$1_4)))
                                             (let
                                                ((rx.0$1_0 _$1_5))
                                                (let
                                                   ((_$1_6 (+ n$1_0_old rx.0$1_0)))
                                                   (let
                                                      ((r.0$1_0 _$1_6))
                                                      (let
                                                         ((result$1 r.0$1_0)
                                                          (_$2_1 (- n$2_0_old 2))
                                                          (_$2_3 (- n$2_0_old 1)))
                                                         (let
                                                            ((_$2_4 (+ n$2_0_old _$2_3)))
                                                            (let
                                                               ((_$2_5 (+ _$2_4 _$2_2)))
                                                               (let
                                                                  ((r.0$2_0 _$2_5))
                                                                  (let
                                                                     ((result$2 r.0$2_0))
                                                                     (= result$1 result$2))))))))))))))))))))))))
; forbidden main
; offbyn main
; end
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (INV_REC_f_PRE n$1_0_old n$2_0_old)
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               _$1_0
               (let
                  ((r.0$1_0 n$1_0_old))
                  (let
                     ((result$1 r.0$1_0)
                      (_$2_0 (<= n$2_0_old 1)))
                     (=>
                        _$2_0
                        (let
                           ((r.0$2_0 n$2_0_old))
                           (let
                              ((result$2 r.0$2_0))
                              (INV_REC_f n$1_0_old n$2_0_old result$1 result$2)))))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (INV_REC_f_PRE n$1_0_old n$2_0_old)
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               _$1_0
               (let
                  ((r.0$1_0 n$1_0_old))
                  (let
                     ((result$1 r.0$1_0)
                      (_$2_0 (<= n$2_0_old 1)))
                     (=>
                        (not _$2_0)
                        (let
                           ((_$2_1 (- n$2_0_old 2)))
                           (and
                              (INV_REC_f__2_PRE _$2_1)
                              (forall
                                 ((_$2_2 Int))
                                 (=>
                                    (INV_REC_f__2 _$2_1 _$2_2)
                                    (let
                                       ((_$2_1 (- n$2_0_old 2))
                                        (_$2_3 (- n$2_0_old 1)))
                                       (let
                                          ((_$2_4 (+ n$2_0_old _$2_3)))
                                          (let
                                             ((_$2_5 (+ _$2_4 _$2_2)))
                                             (let
                                                ((r.0$2_0 _$2_5))
                                                (let
                                                   ((result$2 r.0$2_0))
                                                   (INV_REC_f n$1_0_old n$2_0_old result$1 result$2))))))))))))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (INV_REC_f_PRE n$1_0_old n$2_0_old)
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((_$1_1 (- n$1_0_old 1)))
                  (let
                     ((_$1_2 (<= _$1_1 1)))
                     (=>
                        _$1_2
                        (let
                           ((rx.0$1_0 _$1_1))
                           (let
                              ((_$1_6 (+ n$1_0_old rx.0$1_0)))
                              (let
                                 ((r.0$1_0 _$1_6))
                                 (let
                                    ((result$1 r.0$1_0)
                                     (_$2_0 (<= n$2_0_old 1)))
                                    (=>
                                       _$2_0
                                       (let
                                          ((r.0$2_0 n$2_0_old))
                                          (let
                                             ((result$2 r.0$2_0))
                                             (INV_REC_f n$1_0_old n$2_0_old result$1 result$2))))))))))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (INV_REC_f_PRE n$1_0_old n$2_0_old)
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((_$1_1 (- n$1_0_old 1)))
                  (let
                     ((_$1_2 (<= _$1_1 1)))
                     (=>
                        _$1_2
                        (let
                           ((rx.0$1_0 _$1_1))
                           (let
                              ((_$1_6 (+ n$1_0_old rx.0$1_0)))
                              (let
                                 ((r.0$1_0 _$1_6))
                                 (let
                                    ((result$1 r.0$1_0)
                                     (_$2_0 (<= n$2_0_old 1)))
                                    (=>
                                       (not _$2_0)
                                       (let
                                          ((_$2_1 (- n$2_0_old 2)))
                                          (and
                                             (INV_REC_f__2_PRE _$2_1)
                                             (forall
                                                ((_$2_2 Int))
                                                (=>
                                                   (INV_REC_f__2 _$2_1 _$2_2)
                                                   (let
                                                      ((_$2_1 (- n$2_0_old 2))
                                                       (_$2_3 (- n$2_0_old 1)))
                                                      (let
                                                         ((_$2_4 (+ n$2_0_old _$2_3)))
                                                         (let
                                                            ((_$2_5 (+ _$2_4 _$2_2)))
                                                            (let
                                                               ((r.0$2_0 _$2_5))
                                                               (let
                                                                  ((result$2 r.0$2_0))
                                                                  (INV_REC_f n$1_0_old n$2_0_old result$1 result$2)))))))))))))))))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (INV_REC_f_PRE n$1_0_old n$2_0_old)
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((_$1_1 (- n$1_0_old 1)))
                  (let
                     ((_$1_2 (<= _$1_1 1)))
                     (=>
                        (not _$1_2)
                        (let
                           ((_$1_3 (- _$1_1 1))
                            (_$2_0 (<= n$2_0_old 1)))
                           (=>
                              _$2_0
                              (let
                                 ((r.0$2_0 n$2_0_old))
                                 (let
                                    ((result$2 r.0$2_0))
                                    (and
                                       (INV_REC_f__1_PRE _$1_3)
                                       (forall
                                          ((_$1_4 Int))
                                          (=>
                                             (INV_REC_f__1 _$1_3 _$1_4)
                                             (let
                                                ((_$1_3 (- _$1_1 1))
                                                 (_$1_5 (+ _$1_1 _$1_4)))
                                                (let
                                                   ((rx.0$1_0 _$1_5))
                                                   (let
                                                      ((_$1_6 (+ n$1_0_old rx.0$1_0)))
                                                      (let
                                                         ((r.0$1_0 _$1_6))
                                                         (let
                                                            ((result$1 r.0$1_0))
                                                            (INV_REC_f n$1_0_old n$2_0_old result$1 result$2)))))))))))))))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (INV_REC_f_PRE n$1_0_old n$2_0_old)
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((_$1_1 (- n$1_0_old 1)))
                  (let
                     ((_$1_2 (<= _$1_1 1)))
                     (=>
                        (not _$1_2)
                        (let
                           ((_$1_3 (- _$1_1 1))
                            (_$2_0 (<= n$2_0_old 1)))
                           (=>
                              (not _$2_0)
                              (let
                                 ((_$2_1 (- n$2_0_old 2)))
                                 (and
                                    (INV_REC_f_PRE _$1_3 _$2_1)
                                    (forall
                                       ((_$1_4 Int)
                                        (_$2_2 Int))
                                       (=>
                                          (INV_REC_f _$1_3 _$2_1 _$1_4 _$2_2)
                                          (let
                                             ((_$1_3 (- _$1_1 1))
                                              (_$1_5 (+ _$1_1 _$1_4)))
                                             (let
                                                ((rx.0$1_0 _$1_5))
                                                (let
                                                   ((_$1_6 (+ n$1_0_old rx.0$1_0)))
                                                   (let
                                                      ((r.0$1_0 _$1_6))
                                                      (let
                                                         ((result$1 r.0$1_0)
                                                          (_$2_1 (- n$2_0_old 2))
                                                          (_$2_3 (- n$2_0_old 1)))
                                                         (let
                                                            ((_$2_4 (+ n$2_0_old _$2_3)))
                                                            (let
                                                               ((_$2_5 (+ _$2_4 _$2_2)))
                                                               (let
                                                                  ((r.0$2_0 _$2_5))
                                                                  (let
                                                                     ((result$2 r.0$2_0))
                                                                     (INV_REC_f n$1_0_old n$2_0_old result$1 result$2))))))))))))))))))))))))
(assert
   (forall
      ((n$1_0_old Int))
      (=>
         (INV_REC_f__1_PRE n$1_0_old)
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               _$1_0
               (let
                  ((r.0$1_0 n$1_0_old))
                  (let
                     ((result$1 r.0$1_0))
                     (INV_REC_f__1 n$1_0_old result$1))))))))
(assert
   (forall
      ((n$1_0_old Int))
      (=>
         (INV_REC_f__1_PRE n$1_0_old)
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((_$1_1 (- n$1_0_old 1)))
                  (let
                     ((_$1_2 (<= _$1_1 1)))
                     (=>
                        _$1_2
                        (let
                           ((rx.0$1_0 _$1_1))
                           (let
                              ((_$1_6 (+ n$1_0_old rx.0$1_0)))
                              (let
                                 ((r.0$1_0 _$1_6))
                                 (let
                                    ((result$1 r.0$1_0))
                                    (INV_REC_f__1 n$1_0_old result$1)))))))))))))
(assert
   (forall
      ((n$1_0_old Int))
      (=>
         (INV_REC_f__1_PRE n$1_0_old)
         (let
            ((_$1_0 (<= n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((_$1_1 (- n$1_0_old 1)))
                  (let
                     ((_$1_2 (<= _$1_1 1)))
                     (=>
                        (not _$1_2)
                        (let
                           ((_$1_3 (- _$1_1 1)))
                           (and
                              (INV_REC_f__1_PRE _$1_3)
                              (forall
                                 ((_$1_4 Int))
                                 (=>
                                    (INV_REC_f__1 _$1_3 _$1_4)
                                    (let
                                       ((_$1_3 (- _$1_1 1))
                                        (_$1_5 (+ _$1_1 _$1_4)))
                                       (let
                                          ((rx.0$1_0 _$1_5))
                                          (let
                                             ((_$1_6 (+ n$1_0_old rx.0$1_0)))
                                             (let
                                                ((r.0$1_0 _$1_6))
                                                (let
                                                   ((result$1 r.0$1_0))
                                                   (INV_REC_f__1 n$1_0_old result$1))))))))))))))))))
(assert
   (forall
      ((n$2_0_old Int))
      (=>
         (INV_REC_f__2_PRE n$2_0_old)
         (let
            ((_$2_0 (<= n$2_0_old 1)))
            (=>
               _$2_0
               (let
                  ((r.0$2_0 n$2_0_old))
                  (let
                     ((result$2 r.0$2_0))
                     (INV_REC_f__2 n$2_0_old result$2))))))))
(assert
   (forall
      ((n$2_0_old Int))
      (=>
         (INV_REC_f__2_PRE n$2_0_old)
         (let
            ((_$2_0 (<= n$2_0_old 1)))
            (=>
               (not _$2_0)
               (let
                  ((_$2_1 (- n$2_0_old 2)))
                  (and
                     (INV_REC_f__2_PRE _$2_1)
                     (forall
                        ((_$2_2 Int))
                        (=>
                           (INV_REC_f__2 _$2_1 _$2_2)
                           (let
                              ((_$2_1 (- n$2_0_old 2))
                               (_$2_3 (- n$2_0_old 1)))
                              (let
                                 ((_$2_4 (+ n$2_0_old _$2_3)))
                                 (let
                                    ((_$2_5 (+ _$2_4 _$2_2)))
                                    (let
                                       ((r.0$2_0 _$2_5))
                                       (let
                                          ((result$2 r.0$2_0))
                                          (INV_REC_f__2 n$2_0_old result$2)))))))))))))))
; FORBIDDEN PATHS
; OFF BY N
(check-sat)
(get-model)
