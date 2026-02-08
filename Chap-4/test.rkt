#lang racket

;; 创建一个命名空间用于 eval
(define ns (make-base-namespace))

;; ========== eval 的用法 ==========
;; eval 接受一个表达式（作为数据），并对其求值

(displayln "=== eval 基础用法 ===")

;; 1. 对简单表达式求值（需要命名空间）
(displayln (eval '(+ 1 2) ns))           ; => 3
(displayln (eval '(* 3 4) ns))           ; => 12

;; 2. 对复杂表达式求值
(displayln (eval '(if (> 5 3) "yes" "no") ns))  ; => "yes"

;; 3. 构造表达式并求值
(define op '+)
(define args '(1 2 3))
(displayln (eval (cons op args) ns))     ; => 6，相当于 (+ 1 2 3)


;; ========== apply 的用法 ==========
;; apply 接受一个过程和参数列表，将参数应用到过程

(displayln "\n=== apply 基础用法 ===")

;; 1. 基本用法：将列表作为参数传给函数
(displayln (apply + '(1 2 3 4 5)))    ; => 15，相当于 (+ 1 2 3 4 5)
(displayln (apply * '(1 2 3 4)))      ; => 24，相当于 (* 1 2 3 4)

;; 2. 混合参数：前面是固定参数，最后是列表
(displayln (apply + 1 2 '(3 4 5)))    ; => 15，相当于 (+ 1 2 3 4 5)

;; 3. 配合 max/min 使用
(displayln (apply max '(3 1 4 1 5 9 2 6)))  ; => 9

;; 4. 用 apply 实现 map 的变体（转置矩阵）
(displayln (apply map list '((1 2 3) (a b c))))  ; => ((1 a) (2 b) (3 c))


;; ========== eval 和 apply 结合使用 ==========
(displayln "\n=== eval + apply 结合 ===")

;; 动态构造并执行表达式
(define (make-expr op . operands)
  (cons op operands))

(displayln (eval (make-expr '+ 1 2 3) ns))  ; => 6

;; 用 apply 调用动态获取的过程
(define operations
  (list (cons 'add +)
        (cons 'sub -)
        (cons 'mul *)))

(define (get-op name)
  (cdr (assoc name operations)))

(displayln (apply (get-op 'add) '(10 20 30)))  ; => 60
(displayln (apply (get-op 'mul) '(2 3 4)))     ; => 24


;; ========== 实际应用示例 ==========
(displayln "\n=== 实际应用：简单表达式求值器 ===")

;; 一个简单的表达式求值器，展示 eval 和 apply 的核心思想
(define (my-eval expr)
  (cond
    [(number? expr) expr]                          ; 数字直接返回
    [(list? expr)
     (let ([op (car expr)]                         ; 获取操作符
           [operands (map my-eval (cdr expr))])    ; 递归求值操作数
       (apply (eval op ns) operands))]))           ; 应用操作符到操作数

(displayln (my-eval '(+ 1 (* 2 3))))  ; => 7  (1 + 2*3)
(displayln (my-eval '(- 10 (+ 1 2)))) ; => 7  (10 - (1+2))
(displayln (my-eval '(* (+ 1 2) (- 5 2)))) ; => 9  ((1+2) * (5-2))


;; ========== 总结 ==========
(displayln "\n=== 总结 ===")
(displayln "eval: 把「代码作为数据」求值")
(displayln "  (eval '(+ 1 2) ns) => 对列表 '(+ 1 2) 求值得到 3")
(displayln "")
(displayln "apply: 把「列表展开」作为函数参数")
(displayln "  (apply + '(1 2 3)) => 相当于 (+ 1 2 3) 得到 6")
