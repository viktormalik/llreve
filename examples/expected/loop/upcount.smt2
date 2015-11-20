(set-logic HORN)
(define-fun
   IN_INV
   ((n$1_0 Int)
    (n$2_0 Int))
   Bool
   (and (= n$1_0 n$2_0) (>= n$1_0 0) (>= n$2_0 0)) )
(define-fun
   OUT_INV
   ((result$1 Int)
    (result$2 Int))
   Bool
   (= result$1 result$2))
(declare-fun
   INV_42_MAIN
   (Int
    Int
    Int
    Int)
   Bool)
(declare-fun
   INV_REC_upcount
   (Int
    Int
    Int
    Int)
   Bool)
(declare-fun
   INV_REC_upcount_PRE
   (Int
    Int)
   Bool)
(declare-fun
   INV_REC_upcount__1
   (Int
    Int)
   Bool)
(declare-fun
   INV_REC_upcount__1_PRE
   (Int)
   Bool)
(declare-fun
   INV_REC_upcount__2
   (Int
    Int)
   Bool)
(declare-fun
   INV_REC_upcount__2_PRE
   (Int)
   Bool)
(declare-fun
   INV_42
   (Int
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
    Int)
   Bool)
(declare-fun
   INV_42__1
   (Int
    Int
    Int)
   Bool)
(declare-fun
   INV_42__1_PRE
   (Int
    Int)
   Bool)
(declare-fun
   INV_42__2
   (Int
    Int
    Int)
   Bool)
(declare-fun
   INV_42__2_PRE
   (Int
    Int)
   Bool)
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (IN_INV
            n$1_0_old
            n$2_0_old)
         (let
            ((m.0$1_0 0)
             (.0$1_0 n$1_0_old)
             (m.0$2_0 0)
             (.0$2_0 n$2_0_old))
            (INV_42_MAIN .0$1_0 m.0$1_0 .0$2_0 m.0$2_0)))))
(assert
   (forall
      ((.0$1_0_old Int)
       (m.0$1_0_old Int)
       (.0$2_0_old Int)
       (m.0$2_0_old Int))
      (=>
         (INV_42_MAIN .0$1_0_old m.0$1_0_old .0$2_0_old m.0$2_0_old)
         (let
            ((_$1_1 (>= .0$1_0_old 0)))
            (=>
               (not _$1_1)
               (let
                  ((result$1 m.0$1_0_old)
                   (_$2_1 (> .0$2_0_old 0)))
                  (=>
                     (not _$2_1)
                     (let
                        ((_$2_7 (+ m.0$2_0_old 1)))
                        (let
                           ((result$2 _$2_7))
                           (OUT_INV
                              result$1
                              result$2))))))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (m.0$1_0_old Int)
       (.0$2_0_old Int)
       (m.0$2_0_old Int))
      (=>
         (INV_42_MAIN .0$1_0_old m.0$1_0_old .0$2_0_old m.0$2_0_old)
         (let
            ((_$1_1 (>= .0$1_0_old 0)))
            (=>
               _$1_1
               (let
                  ((_$1_5 (+ m.0$1_0_old 1))
                   (_$1_6 (+ .0$1_0_old (- 1))))
                  (let
                     ((m.0$1_0 _$1_5)
                      (.0$1_0 _$1_6)
                      (_$2_1 (> .0$2_0_old 0)))
                     (=>
                        _$2_1
                        (let
                           ((_$2_5 (+ m.0$2_0_old 1))
                            (_$2_6 (+ .0$2_0_old (- 1))))
                           (let
                              ((m.0$2_0 _$2_5)
                               (.0$2_0 _$2_6))
                              (INV_42_MAIN .0$1_0 m.0$1_0 .0$2_0 m.0$2_0)))))))))))
; forbidden main
; offbyn main
(assert
   (forall
      ((.0$1_0_old Int)
       (m.0$1_0_old Int)
       (.0$2_0_old Int)
       (m.0$2_0_old Int))
      (=>
         (INV_42_MAIN .0$1_0_old m.0$1_0_old .0$2_0_old m.0$2_0_old)
         (let
            ((_$1_1 (>= .0$1_0_old 0)))
            (=>
               _$1_1
               (let
                  ((_$1_5 (+ m.0$1_0_old 1))
                   (_$1_6 (+ .0$1_0_old (- 1))))
                  (let
                     ((m.0$1_0 _$1_5)
                      (.0$1_0 _$1_6))
                     (=>
                        (and
                           (let
                              ((_$2_1 (> .0$2_0_old 0)))
                              (=>
                                 _$2_1
                                 (let
                                    ((_$2_5 (+ m.0$2_0_old 1))
                                     (_$2_6 (+ .0$2_0_old (- 1))))
                                    (let
                                       ((m.0$2_0 _$2_5)
                                        (.0$2_0 _$2_6))
                                       false)))))
                        (INV_42_MAIN .0$1_0 m.0$1_0 .0$2_0_old m.0$2_0_old)))))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (m.0$1_0_old Int)
       (.0$2_0_old Int)
       (m.0$2_0_old Int))
      (=>
         (INV_42_MAIN .0$1_0_old m.0$1_0_old .0$2_0_old m.0$2_0_old)
         (let
            ((_$2_1 (> .0$2_0_old 0)))
            (=>
               _$2_1
               (let
                  ((_$2_5 (+ m.0$2_0_old 1))
                   (_$2_6 (+ .0$2_0_old (- 1))))
                  (let
                     ((m.0$2_0 _$2_5)
                      (.0$2_0 _$2_6))
                     (=>
                        (and
                           (let
                              ((_$1_1 (>= .0$1_0_old 0)))
                              (=>
                                 _$1_1
                                 (let
                                    ((_$1_5 (+ m.0$1_0_old 1))
                                     (_$1_6 (+ .0$1_0_old (- 1))))
                                    (let
                                       ((m.0$1_0 _$1_5)
                                        (.0$1_0 _$1_6))
                                       false)))))
                        (INV_42_MAIN .0$1_0_old m.0$1_0_old .0$2_0 m.0$2_0)))))))))
; end
(assert
   (forall
      ((n$1_0_old Int)
       (n$2_0_old Int))
      (=>
         (INV_REC_upcount_PRE n$1_0_old n$2_0_old)
         (let
            ((m.0$1_0 0)
             (.0$1_0 n$1_0_old)
             (m.0$2_0 0)
             (.0$2_0 n$2_0_old))
            (forall
               ((result$1 Int)
                (result$2 Int))
               (and
                  (INV_42_PRE .0$1_0 m.0$1_0 .0$2_0 m.0$2_0)
                  (=>
                     (INV_42 .0$1_0 m.0$1_0 .0$2_0 m.0$2_0 result$1 result$2)
                     (INV_REC_upcount n$1_0_old n$2_0_old result$1 result$2))))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (m.0$1_0_old Int)
       (.0$2_0_old Int)
       (m.0$2_0_old Int))
      (=>
         (INV_42_PRE .0$1_0_old m.0$1_0_old .0$2_0_old m.0$2_0_old)
         (let
            ((_$1_1 (>= .0$1_0_old 0)))
            (=>
               (not _$1_1)
               (let
                  ((result$1 m.0$1_0_old)
                   (_$2_1 (> .0$2_0_old 0)))
                  (=>
                     (not _$2_1)
                     (let
                        ((_$2_7 (+ m.0$2_0_old 1)))
                        (let
                           ((result$2 _$2_7))
                           (INV_42 .0$1_0_old m.0$1_0_old .0$2_0_old m.0$2_0_old result$1 result$2))))))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (m.0$1_0_old Int)
       (.0$2_0_old Int)
       (m.0$2_0_old Int))
      (=>
         (INV_42_PRE .0$1_0_old m.0$1_0_old .0$2_0_old m.0$2_0_old)
         (let
            ((_$1_1 (>= .0$1_0_old 0)))
            (=>
               _$1_1
               (let
                  ((_$1_5 (+ m.0$1_0_old 1))
                   (_$1_6 (+ .0$1_0_old (- 1))))
                  (let
                     ((m.0$1_0 _$1_5)
                      (.0$1_0 _$1_6)
                      (_$2_1 (> .0$2_0_old 0)))
                     (=>
                        _$2_1
                        (let
                           ((_$2_5 (+ m.0$2_0_old 1))
                            (_$2_6 (+ .0$2_0_old (- 1))))
                           (let
                              ((m.0$2_0 _$2_5)
                               (.0$2_0 _$2_6))
                              (forall
                                 ((result$1 Int)
                                  (result$2 Int))
                                 (and
                                    (INV_42_PRE .0$1_0 m.0$1_0 .0$2_0 m.0$2_0)
                                    (=>
                                       (INV_42 .0$1_0 m.0$1_0 .0$2_0 m.0$2_0 result$1 result$2)
                                       (INV_42 .0$1_0_old m.0$1_0_old .0$2_0_old m.0$2_0_old result$1 result$2))))))))))))))
(assert
   (forall
      ((n$1_0_old Int))
      (=>
         (INV_REC_upcount__1_PRE n$1_0_old)
         (let
            ((m.0$1_0 0)
             (.0$1_0 n$1_0_old))
            (forall
               ((result$1 Int))
               (=>
                  (INV_42__1 .0$1_0 m.0$1_0 result$1)
                  (INV_REC_upcount__1 n$1_0_old result$1)))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (m.0$1_0_old Int))
      (=>
         (INV_42__1_PRE .0$1_0_old m.0$1_0_old)
         (let
            ((_$1_1 (>= .0$1_0_old 0)))
            (=>
               (not _$1_1)
               (let
                  ((result$1 m.0$1_0_old))
                  (INV_42__1 .0$1_0_old m.0$1_0_old result$1)))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (m.0$1_0_old Int))
      (=>
         (INV_42__1_PRE .0$1_0_old m.0$1_0_old)
         (let
            ((_$1_1 (>= .0$1_0_old 0)))
            (=>
               _$1_1
               (let
                  ((_$1_5 (+ m.0$1_0_old 1))
                   (_$1_6 (+ .0$1_0_old (- 1))))
                  (let
                     ((m.0$1_0 _$1_5)
                      (.0$1_0 _$1_6))
                     (forall
                        ((result$1 Int))
                        (=>
                           (INV_42__1 .0$1_0 m.0$1_0 result$1)
                           (INV_42__1 .0$1_0_old m.0$1_0_old result$1))))))))))
(assert
   (forall
      ((n$2_0_old Int))
      (=>
         (INV_REC_upcount__2_PRE n$2_0_old)
         (let
            ((m.0$2_0 0)
             (.0$2_0 n$2_0_old))
            (forall
               ((result$2 Int))
               (=>
                  (INV_42__2 .0$2_0 m.0$2_0 result$2)
                  (INV_REC_upcount__2 n$2_0_old result$2)))))))
(assert
   (forall
      ((.0$2_0_old Int)
       (m.0$2_0_old Int))
      (=>
         (INV_42__2_PRE .0$2_0_old m.0$2_0_old)
         (let
            ((_$2_1 (> .0$2_0_old 0)))
            (=>
               (not _$2_1)
               (let
                  ((_$2_7 (+ m.0$2_0_old 1)))
                  (let
                     ((result$2 _$2_7))
                     (INV_42__2 .0$2_0_old m.0$2_0_old result$2))))))))
(assert
   (forall
      ((.0$2_0_old Int)
       (m.0$2_0_old Int))
      (=>
         (INV_42__2_PRE .0$2_0_old m.0$2_0_old)
         (let
            ((_$2_1 (> .0$2_0_old 0)))
            (=>
               _$2_1
               (let
                  ((_$2_5 (+ m.0$2_0_old 1))
                   (_$2_6 (+ .0$2_0_old (- 1))))
                  (let
                     ((m.0$2_0 _$2_5)
                      (.0$2_0 _$2_6))
                     (forall
                        ((result$2 Int))
                        (=>
                           (INV_42__2 .0$2_0 m.0$2_0 result$2)
                           (INV_42__2 .0$2_0_old m.0$2_0_old result$2))))))))))
; FORBIDDEN PATHS
; OFF BY N
(assert
   (forall
      ((.0$1_0_old Int)
       (m.0$1_0_old Int)
       (.0$2_0_old Int)
       (m.0$2_0_old Int))
      (=>
         (INV_42_PRE .0$1_0_old m.0$1_0_old .0$2_0_old m.0$2_0_old)
         (let
            ((_$1_1 (>= .0$1_0_old 0)))
            (=>
               _$1_1
               (let
                  ((_$1_5 (+ m.0$1_0_old 1))
                   (_$1_6 (+ .0$1_0_old (- 1))))
                  (let
                     ((m.0$1_0 _$1_5)
                      (.0$1_0 _$1_6))
                     (=>
                        (and
                           (let
                              ((_$2_1 (> .0$2_0_old 0)))
                              (=>
                                 _$2_1
                                 (let
                                    ((_$2_5 (+ m.0$2_0_old 1))
                                     (_$2_6 (+ .0$2_0_old (- 1))))
                                    (let
                                       ((m.0$2_0 _$2_5)
                                        (.0$2_0 _$2_6))
                                       false)))))
                        (forall
                           ((result$1 Int)
                            (result$2 Int))
                           (and
                              (INV_42_PRE .0$1_0 m.0$1_0 .0$2_0_old m.0$2_0_old)
                              (=>
                                 (INV_42 .0$1_0 m.0$1_0 .0$2_0_old m.0$2_0_old result$1 result$2)
                                 (INV_42 .0$1_0_old m.0$1_0_old .0$2_0_old m.0$2_0_old result$1 result$2))))))))))))
(assert
   (forall
      ((.0$1_0_old Int)
       (m.0$1_0_old Int)
       (.0$2_0_old Int)
       (m.0$2_0_old Int))
      (=>
         (INV_42_PRE .0$1_0_old m.0$1_0_old .0$2_0_old m.0$2_0_old)
         (let
            ((_$2_1 (> .0$2_0_old 0)))
            (=>
               _$2_1
               (let
                  ((_$2_5 (+ m.0$2_0_old 1))
                   (_$2_6 (+ .0$2_0_old (- 1))))
                  (let
                     ((m.0$2_0 _$2_5)
                      (.0$2_0 _$2_6))
                     (=>
                        (and
                           (let
                              ((_$1_1 (>= .0$1_0_old 0)))
                              (=>
                                 _$1_1
                                 (let
                                    ((_$1_5 (+ m.0$1_0_old 1))
                                     (_$1_6 (+ .0$1_0_old (- 1))))
                                    (let
                                       ((m.0$1_0 _$1_5)
                                        (.0$1_0 _$1_6))
                                       false)))))
                        (forall
                           ((result$1 Int)
                            (result$2 Int))
                           (and
                              (INV_42_PRE .0$1_0_old m.0$1_0_old .0$2_0 m.0$2_0)
                              (=>
                                 (INV_42 .0$1_0_old m.0$1_0_old .0$2_0 m.0$2_0 result$1 result$2)
                                 (INV_42 .0$1_0_old m.0$1_0_old .0$2_0_old m.0$2_0_old result$1 result$2))))))))))))
(check-sat)
(get-model)
