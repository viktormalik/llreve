(set-logic HORN)
(declare-fun
   INV_23_MAIN
   (Int
    Int)
   Bool)
(declare-fun
   INV_42_MAIN
   (Int
    Int
    Int
    Int
    Int
    Int)
   Bool)
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
(declare-fun
   INV_23
   (Int
    Int
    Int
    Int)
   Bool)
(declare-fun
   INV_23_PRE
   (Int
    Int)
   Bool)
(declare-fun
   INV_42
   (Int
    Int
    Int
    Int
    Int
    Int
    Int
    Int)
   Bool)
(declare-fun
   INV_42_PRE
   (Int
    Int
    Int
    Int
    Int
    Int)
   Bool)
(declare-fun
   INV_23__1
   (Int
    Int)
   Bool)
(declare-fun
   INV_23__1_PRE
   (Int)
   Bool)
(declare-fun
   INV_42__1
   (Int
    Int
    Int
    Int)
   Bool)
(declare-fun
   INV_42__1_PRE
   (Int
    Int
    Int)
   Bool)
(declare-fun
   INV_23__2
   (Int
    Int)
   Bool)
(declare-fun
   INV_23__2_PRE
   (Int)
   Bool)
(declare-fun
   INV_42__2
   (Int
    Int
    Int
    Int)
   Bool)
(declare-fun
   INV_42__2_PRE
   (Int
    Int
    Int)
   Bool)
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (and
            (= n$1_0_old n$2_0_old))
         (let
            ((_$1_0 (< n$1_0_old 1)))
            (=>
               _$1_0
               (let
                  ((.0$1_0 1)
                   (_$2_0 (< n$2_0_old 1)))
                  (=>
                     _$2_0
                     (let
                        ((.0$2_0 1))
                        (INV_23_MAIN .0$1_0 .0$2_0)))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (and
            (= n$1_0_old n$2_0_old))
         (let
            ((_$1_0 (< n$1_0_old 1)))
            (=>
               _$1_0
               (let
                  ((.0$1_0 1)
                   (_$2_0 (< n$2_0_old 1)))
                  (=>
                     (not _$2_0)
                     (let
                        ((.0$2_0 n$2_0_old))
                        (INV_23_MAIN .0$1_0 .0$2_0)))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (and
            (= n$1_0_old n$2_0_old))
         (let
            ((_$1_0 (< n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((.0$1_0 n$1_0_old)
                   (_$2_0 (< n$2_0_old 1)))
                  (=>
                     _$2_0
                     (let
                        ((.0$2_0 1))
                        (INV_23_MAIN .0$1_0 .0$2_0)))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (and
            (= n$1_0_old n$2_0_old))
         (let
            ((_$1_0 (< n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((.0$1_0 n$1_0_old)
                   (_$2_0 (< n$2_0_old 1)))
                  (=>
                     (not _$2_0)
                     (let
                        ((.0$2_0 n$2_0_old))
                        (INV_23_MAIN .0$1_0 .0$2_0)))))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (.0$2_0_old Int))
      (=>
         (INV_23_MAIN .0$1_0_old .0$2_0_old)
         (let
            ((j.0$1_0 0)
             (i.0$1_0 1)
             (.0$1_0 .0$1_0_old)
             (j.0$2_0 2)
             (i.0$2_0 1)
             (.0$2_0 .0$2_0_old))
            (INV_42_MAIN .0$1_0 i.0$1_0 j.0$1_0 .0$2_0 i.0$2_0 j.0$2_0)))))
(assert
   (forall
      ((.0$1_0_old Int)
       (i.0$1_0_old Int)
       (j.0$1_0_old Int)
       (.0$2_0_old Int)
       (i.0$2_0_old Int)
       (j.0$2_0_old Int))
      (=>
         (INV_42_MAIN .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0_old i.0$2_0_old j.0$2_0_old)
         (let
            ((_$1_3 (<= i.0$1_0_old .0$1_0_old)))
            (=>
               (not _$1_3)
               (let
                  ((result$1 j.0$1_0_old)
                   (_$2_3 (< i.0$2_0_old .0$2_0_old)))
                  (=>
                     (not _$2_3)
                     (let
                        ((result$2 j.0$2_0_old))
                        (= result$1 result$2)))))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (i.0$1_0_old Int)
       (j.0$1_0_old Int)
       (.0$2_0_old Int)
       (i.0$2_0_old Int)
       (j.0$2_0_old Int))
      (=>
         (INV_42_MAIN .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0_old i.0$2_0_old j.0$2_0_old)
         (let
            ((_$1_3 (<= i.0$1_0_old .0$1_0_old)))
            (=>
               _$1_3
               (let
                  ((_$1_7 (+ j.0$1_0_old 2))
                   (_$1_8 (+ i.0$1_0_old 1)))
                  (let
                     ((j.0$1_0 _$1_7)
                      (i.0$1_0 _$1_8)
                      (.0$1_0 .0$1_0_old)
                      (_$2_3 (< i.0$2_0_old .0$2_0_old)))
                     (=>
                        _$2_3
                        (let
                           ((_$2_7 (+ j.0$2_0_old 2))
                            (_$2_8 (+ i.0$2_0_old 1)))
                           (let
                              ((j.0$2_0 _$2_7)
                               (i.0$2_0 _$2_8)
                               (.0$2_0 .0$2_0_old))
                              (INV_42_MAIN .0$1_0 i.0$1_0 j.0$1_0 .0$2_0 i.0$2_0 j.0$2_0)))))))))))
; forbidden main
; offbyn main
(assert
   (forall
      ((.0$1_0_old Int)
       (i.0$1_0_old Int)
       (j.0$1_0_old Int)
       (.0$2_0_old Int)
       (i.0$2_0_old Int)
       (j.0$2_0_old Int))
      (=>
         (INV_42_MAIN .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0_old i.0$2_0_old j.0$2_0_old)
         (let
            ((_$1_3 (<= i.0$1_0_old .0$1_0_old)))
            (=>
               _$1_3
               (let
                  ((_$1_7 (+ j.0$1_0_old 2))
                   (_$1_8 (+ i.0$1_0_old 1)))
                  (let
                     ((j.0$1_0 _$1_7)
                      (i.0$1_0 _$1_8)
                      (.0$1_0 .0$1_0_old))
                     (=>
                        (and
                           (let
                              ((_$2_3 (< i.0$2_0_old .0$2_0_old)))
                              (=>
                                 _$2_3
                                 (let
                                    ((_$2_7 (+ j.0$2_0_old 2))
                                     (_$2_8 (+ i.0$2_0_old 1)))
                                    (let
                                       ((j.0$2_0 _$2_7)
                                        (i.0$2_0 _$2_8)
                                        (.0$2_0 .0$2_0_old))
                                       false)))))
                        (INV_42_MAIN .0$1_0 i.0$1_0 j.0$1_0 .0$2_0_old i.0$2_0_old j.0$2_0_old)))))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (i.0$1_0_old Int)
       (j.0$1_0_old Int)
       (.0$2_0_old Int)
       (i.0$2_0_old Int)
       (j.0$2_0_old Int))
      (=>
         (INV_42_MAIN .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0_old i.0$2_0_old j.0$2_0_old)
         (let
            ((_$2_3 (< i.0$2_0_old .0$2_0_old)))
            (=>
               _$2_3
               (let
                  ((_$2_7 (+ j.0$2_0_old 2))
                   (_$2_8 (+ i.0$2_0_old 1)))
                  (let
                     ((j.0$2_0 _$2_7)
                      (i.0$2_0 _$2_8)
                      (.0$2_0 .0$2_0_old))
                     (=>
                        (and
                           (let
                              ((_$1_3 (<= i.0$1_0_old .0$1_0_old)))
                              (=>
                                 _$1_3
                                 (let
                                    ((_$1_7 (+ j.0$1_0_old 2))
                                     (_$1_8 (+ i.0$1_0_old 1)))
                                    (let
                                       ((j.0$1_0 _$1_7)
                                        (i.0$1_0 _$1_8)
                                        (.0$1_0 .0$1_0_old))
                                       false)))))
                        (INV_42_MAIN .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0 i.0$2_0 j.0$2_0)))))))))
; end
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (INV_REC_f_PRE n$1_0_old n$2_0_old)
         (let
            ((_$1_0 (< n$1_0_old 1)))
            (=>
               _$1_0
               (let
                  ((.0$1_0 1)
                   (_$2_0 (< n$2_0_old 1)))
                  (=>
                     _$2_0
                     (let
                        ((.0$2_0 1))
                        (forall
                           ((result$1 Int)
                            (result$2 Int))
                           (and
                              (INV_23_PRE .0$1_0 .0$2_0)
                              (=>
                                 (INV_23 .0$1_0 .0$2_0 result$1 result$2)
                                 (INV_REC_f n$1_0_old n$2_0_old result$1 result$2))))))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (INV_REC_f_PRE n$1_0_old n$2_0_old)
         (let
            ((_$1_0 (< n$1_0_old 1)))
            (=>
               _$1_0
               (let
                  ((.0$1_0 1)
                   (_$2_0 (< n$2_0_old 1)))
                  (=>
                     (not _$2_0)
                     (let
                        ((.0$2_0 n$2_0_old))
                        (forall
                           ((result$1 Int)
                            (result$2 Int))
                           (and
                              (INV_23_PRE .0$1_0 .0$2_0)
                              (=>
                                 (INV_23 .0$1_0 .0$2_0 result$1 result$2)
                                 (INV_REC_f n$1_0_old n$2_0_old result$1 result$2))))))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (INV_REC_f_PRE n$1_0_old n$2_0_old)
         (let
            ((_$1_0 (< n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((.0$1_0 n$1_0_old)
                   (_$2_0 (< n$2_0_old 1)))
                  (=>
                     _$2_0
                     (let
                        ((.0$2_0 1))
                        (forall
                           ((result$1 Int)
                            (result$2 Int))
                           (and
                              (INV_23_PRE .0$1_0 .0$2_0)
                              (=>
                                 (INV_23 .0$1_0 .0$2_0 result$1 result$2)
                                 (INV_REC_f n$1_0_old n$2_0_old result$1 result$2))))))))))))
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (INV_REC_f_PRE n$1_0_old n$2_0_old)
         (let
            ((_$1_0 (< n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((.0$1_0 n$1_0_old)
                   (_$2_0 (< n$2_0_old 1)))
                  (=>
                     (not _$2_0)
                     (let
                        ((.0$2_0 n$2_0_old))
                        (forall
                           ((result$1 Int)
                            (result$2 Int))
                           (and
                              (INV_23_PRE .0$1_0 .0$2_0)
                              (=>
                                 (INV_23 .0$1_0 .0$2_0 result$1 result$2)
                                 (INV_REC_f n$1_0_old n$2_0_old result$1 result$2))))))))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (.0$2_0_old Int))
      (=>
         (INV_23_PRE .0$1_0_old .0$2_0_old)
         (let
            ((j.0$1_0 0)
             (i.0$1_0 1)
             (.0$1_0 .0$1_0_old)
             (j.0$2_0 2)
             (i.0$2_0 1)
             (.0$2_0 .0$2_0_old))
            (forall
               ((result$1 Int)
                (result$2 Int))
               (and
                  (INV_42_PRE .0$1_0 i.0$1_0 j.0$1_0 .0$2_0 i.0$2_0 j.0$2_0)
                  (=>
                     (INV_42 .0$1_0 i.0$1_0 j.0$1_0 .0$2_0 i.0$2_0 j.0$2_0 result$1 result$2)
                     (INV_23 .0$1_0_old .0$2_0_old result$1 result$2))))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (i.0$1_0_old Int)
       (j.0$1_0_old Int)
       (.0$2_0_old Int)
       (i.0$2_0_old Int)
       (j.0$2_0_old Int))
      (=>
         (INV_42_PRE .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0_old i.0$2_0_old j.0$2_0_old)
         (let
            ((_$1_3 (<= i.0$1_0_old .0$1_0_old)))
            (=>
               (not _$1_3)
               (let
                  ((result$1 j.0$1_0_old)
                   (_$2_3 (< i.0$2_0_old .0$2_0_old)))
                  (=>
                     (not _$2_3)
                     (let
                        ((result$2 j.0$2_0_old))
                        (INV_42 .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0_old i.0$2_0_old j.0$2_0_old result$1 result$2)))))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (i.0$1_0_old Int)
       (j.0$1_0_old Int)
       (.0$2_0_old Int)
       (i.0$2_0_old Int)
       (j.0$2_0_old Int))
      (=>
         (INV_42_PRE .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0_old i.0$2_0_old j.0$2_0_old)
         (let
            ((_$1_3 (<= i.0$1_0_old .0$1_0_old)))
            (=>
               _$1_3
               (let
                  ((_$1_7 (+ j.0$1_0_old 2))
                   (_$1_8 (+ i.0$1_0_old 1)))
                  (let
                     ((j.0$1_0 _$1_7)
                      (i.0$1_0 _$1_8)
                      (.0$1_0 .0$1_0_old)
                      (_$2_3 (< i.0$2_0_old .0$2_0_old)))
                     (=>
                        _$2_3
                        (let
                           ((_$2_7 (+ j.0$2_0_old 2))
                            (_$2_8 (+ i.0$2_0_old 1)))
                           (let
                              ((j.0$2_0 _$2_7)
                               (i.0$2_0 _$2_8)
                               (.0$2_0 .0$2_0_old))
                              (forall
                                 ((result$1 Int)
                                  (result$2 Int))
                                 (and
                                    (INV_42_PRE .0$1_0 i.0$1_0 j.0$1_0 .0$2_0 i.0$2_0 j.0$2_0)
                                    (=>
                                       (INV_42 .0$1_0 i.0$1_0 j.0$1_0 .0$2_0 i.0$2_0 j.0$2_0 result$1 result$2)
                                       (INV_42 .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0_old i.0$2_0_old j.0$2_0_old result$1 result$2))))))))))))))
(assert
   (forall
      ((n$1_0_old Int))
      (=>
         (INV_REC_f__1_PRE n$1_0_old)
         (let
            ((_$1_0 (< n$1_0_old 1)))
            (=>
               _$1_0
               (let
                  ((.0$1_0 1))
                  (forall
                     ((result$1 Int))
                     (=>
                        (INV_23__1 .0$1_0 result$1)
                        (INV_REC_f__1 n$1_0_old result$1)))))))))
(assert
   (forall
      ((n$1_0_old Int))
      (=>
         (INV_REC_f__1_PRE n$1_0_old)
         (let
            ((_$1_0 (< n$1_0_old 1)))
            (=>
               (not _$1_0)
               (let
                  ((.0$1_0 n$1_0_old))
                  (forall
                     ((result$1 Int))
                     (=>
                        (INV_23__1 .0$1_0 result$1)
                        (INV_REC_f__1 n$1_0_old result$1)))))))))
(assert
   (forall
      ((.0$1_0_old Int))
      (=>
         (INV_23__1_PRE .0$1_0_old)
         (let
            ((j.0$1_0 0)
             (i.0$1_0 1)
             (.0$1_0 .0$1_0_old))
            (forall
               ((result$1 Int))
               (=>
                  (INV_42__1 .0$1_0 i.0$1_0 j.0$1_0 result$1)
                  (INV_23__1 .0$1_0_old result$1)))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (i.0$1_0_old Int)
       (j.0$1_0_old Int))
      (=>
         (INV_42__1_PRE .0$1_0_old i.0$1_0_old j.0$1_0_old)
         (let
            ((_$1_3 (<= i.0$1_0_old .0$1_0_old)))
            (=>
               (not _$1_3)
               (let
                  ((result$1 j.0$1_0_old))
                  (INV_42__1 .0$1_0_old i.0$1_0_old j.0$1_0_old result$1)))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (i.0$1_0_old Int)
       (j.0$1_0_old Int))
      (=>
         (INV_42__1_PRE .0$1_0_old i.0$1_0_old j.0$1_0_old)
         (let
            ((_$1_3 (<= i.0$1_0_old .0$1_0_old)))
            (=>
               _$1_3
               (let
                  ((_$1_7 (+ j.0$1_0_old 2))
                   (_$1_8 (+ i.0$1_0_old 1)))
                  (let
                     ((j.0$1_0 _$1_7)
                      (i.0$1_0 _$1_8)
                      (.0$1_0 .0$1_0_old))
                     (forall
                        ((result$1 Int))
                        (=>
                           (INV_42__1 .0$1_0 i.0$1_0 j.0$1_0 result$1)
                           (INV_42__1 .0$1_0_old i.0$1_0_old j.0$1_0_old result$1))))))))))
(assert
   (forall
      ((n$2_0_old Int))
      (=>
         (INV_REC_f__2_PRE n$2_0_old)
         (let
            ((_$2_0 (< n$2_0_old 1)))
            (=>
               _$2_0
               (let
                  ((.0$2_0 1))
                  (forall
                     ((result$2 Int))
                     (=>
                        (INV_23__2 .0$2_0 result$2)
                        (INV_REC_f__2 n$2_0_old result$2)))))))))
(assert
   (forall
      ((n$2_0_old Int))
      (=>
         (INV_REC_f__2_PRE n$2_0_old)
         (let
            ((_$2_0 (< n$2_0_old 1)))
            (=>
               (not _$2_0)
               (let
                  ((.0$2_0 n$2_0_old))
                  (forall
                     ((result$2 Int))
                     (=>
                        (INV_23__2 .0$2_0 result$2)
                        (INV_REC_f__2 n$2_0_old result$2)))))))))
(assert
   (forall
      ((.0$2_0_old Int))
      (=>
         (INV_23__2_PRE .0$2_0_old)
         (let
            ((j.0$2_0 2)
             (i.0$2_0 1)
             (.0$2_0 .0$2_0_old))
            (forall
               ((result$2 Int))
               (=>
                  (INV_42__2 .0$2_0 i.0$2_0 j.0$2_0 result$2)
                  (INV_23__2 .0$2_0_old result$2)))))))
(assert
   (forall
      ((.0$2_0_old Int)
       (i.0$2_0_old Int)
       (j.0$2_0_old Int))
      (=>
         (INV_42__2_PRE .0$2_0_old i.0$2_0_old j.0$2_0_old)
         (let
            ((_$2_3 (< i.0$2_0_old .0$2_0_old)))
            (=>
               (not _$2_3)
               (let
                  ((result$2 j.0$2_0_old))
                  (INV_42__2 .0$2_0_old i.0$2_0_old j.0$2_0_old result$2)))))))
(assert
   (forall
      ((.0$2_0_old Int)
       (i.0$2_0_old Int)
       (j.0$2_0_old Int))
      (=>
         (INV_42__2_PRE .0$2_0_old i.0$2_0_old j.0$2_0_old)
         (let
            ((_$2_3 (< i.0$2_0_old .0$2_0_old)))
            (=>
               _$2_3
               (let
                  ((_$2_7 (+ j.0$2_0_old 2))
                   (_$2_8 (+ i.0$2_0_old 1)))
                  (let
                     ((j.0$2_0 _$2_7)
                      (i.0$2_0 _$2_8)
                      (.0$2_0 .0$2_0_old))
                     (forall
                        ((result$2 Int))
                        (=>
                           (INV_42__2 .0$2_0 i.0$2_0 j.0$2_0 result$2)
                           (INV_42__2 .0$2_0_old i.0$2_0_old j.0$2_0_old result$2))))))))))
; FORBIDDEN PATHS
; OFF BY N
(assert
   (forall
      ((.0$1_0_old Int)
       (i.0$1_0_old Int)
       (j.0$1_0_old Int)
       (.0$2_0_old Int)
       (i.0$2_0_old Int)
       (j.0$2_0_old Int))
      (=>
         (INV_42_PRE .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0_old i.0$2_0_old j.0$2_0_old)
         (let
            ((_$1_3 (<= i.0$1_0_old .0$1_0_old)))
            (=>
               _$1_3
               (let
                  ((_$1_7 (+ j.0$1_0_old 2))
                   (_$1_8 (+ i.0$1_0_old 1)))
                  (let
                     ((j.0$1_0 _$1_7)
                      (i.0$1_0 _$1_8)
                      (.0$1_0 .0$1_0_old))
                     (=>
                        (and
                           (let
                              ((_$2_3 (< i.0$2_0_old .0$2_0_old)))
                              (=>
                                 _$2_3
                                 (let
                                    ((_$2_7 (+ j.0$2_0_old 2))
                                     (_$2_8 (+ i.0$2_0_old 1)))
                                    (let
                                       ((j.0$2_0 _$2_7)
                                        (i.0$2_0 _$2_8)
                                        (.0$2_0 .0$2_0_old))
                                       false)))))
                        (forall
                           ((result$1 Int)
                            (result$2 Int))
                           (and
                              (INV_42_PRE .0$1_0 i.0$1_0 j.0$1_0 .0$2_0_old i.0$2_0_old j.0$2_0_old)
                              (=>
                                 (INV_42 .0$1_0 i.0$1_0 j.0$1_0 .0$2_0_old i.0$2_0_old j.0$2_0_old result$1 result$2)
                                 (INV_42 .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0_old i.0$2_0_old j.0$2_0_old result$1 result$2))))))))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (i.0$1_0_old Int)
       (j.0$1_0_old Int)
       (.0$2_0_old Int)
       (i.0$2_0_old Int)
       (j.0$2_0_old Int))
      (=>
         (INV_42_PRE .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0_old i.0$2_0_old j.0$2_0_old)
         (let
            ((_$2_3 (< i.0$2_0_old .0$2_0_old)))
            (=>
               _$2_3
               (let
                  ((_$2_7 (+ j.0$2_0_old 2))
                   (_$2_8 (+ i.0$2_0_old 1)))
                  (let
                     ((j.0$2_0 _$2_7)
                      (i.0$2_0 _$2_8)
                      (.0$2_0 .0$2_0_old))
                     (=>
                        (and
                           (let
                              ((_$1_3 (<= i.0$1_0_old .0$1_0_old)))
                              (=>
                                 _$1_3
                                 (let
                                    ((_$1_7 (+ j.0$1_0_old 2))
                                     (_$1_8 (+ i.0$1_0_old 1)))
                                    (let
                                       ((j.0$1_0 _$1_7)
                                        (i.0$1_0 _$1_8)
                                        (.0$1_0 .0$1_0_old))
                                       false)))))
                        (forall
                           ((result$1 Int)
                            (result$2 Int))
                           (and
                              (INV_42_PRE .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0 i.0$2_0 j.0$2_0)
                              (=>
                                 (INV_42 .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0 i.0$2_0 j.0$2_0 result$1 result$2)
                                 (INV_42 .0$1_0_old i.0$1_0_old j.0$1_0_old .0$2_0_old i.0$2_0_old j.0$2_0_old result$1 result$2))))))))))))
(check-sat)
(get-model)