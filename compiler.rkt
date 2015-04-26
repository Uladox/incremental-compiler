#lang racket

(require racket/cmdline)
 (define compile-bits 32)

(command-line
 #:once-any
 [("--32") "32 bit compile" (set! compile-bits 32)]
 [("--64") "64 bit compile" (set! compile-bits 64)]
 #:help-labels "operations to perform:")

(define nil_t #x3F)

(define bool_f #x2F)
(define bool_t #x6F)
(define bool_bit #x40)
(define bool_bit_pos 6)

(define fixnum_tag #x00)
(define fixnum_tag_size 2)
(define fixnum_tag_mask #x03)
(define char_tag #x0F)

(define wordsize 4)
(if (eq? compile-bits 32)
    (set! wordsize  4)
    (if (eq? compile-bits 64)
        (set! wordsize 8)
        "error")) ; bytes

(define outputed-procedures "")

(define (update-outputed-procedures new-proc)
  (set! outputed-procedures (string-append new-proc outputed-procedures)))

(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L~s" count)])
        (set! count (add1 count))
        L))))

(define (to_fixnum x)
  (arithmetic-shift x fixnum_tag_size))

(define (to_char x)
  (bitwise-ior
   (arithmetic-shift (char->integer x) 8) char_tag))

(define (fixnum? x)
  (integer? x))

(define (boolean? x)
  (or (eq? x bool_t) (eq? x bool_f)))

(define (char_t? x)
  (if (integer? x)
      (eq? (arithmetic-shift x -8) char_tag)
      #f))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char_t? x)))

(define (if? x)
  (and (list? x) (eq? (car x) 'if)))

(define (and? x)
  (and (list? x) (eq? (car x) 'and)))

(define (primcall? x)
  (and
   (pair? x)
   (member
    (car x)
    '(not add1 sub1 zero? bool? null? char? fixnum?))))

(define (binary-prim? x)
  (and
   (list? x)
   (member
    (car x)
    '(+ - *))))

(define (let? x)
  (and
   (list? x)
   (eq? (car x) 'let)))

(define (apply? x)
  (and
   (list? x)
   (eq? (car x) 'apply)))

(define (lambda? x)
  (and
   (list? x)
   (eq? (car x) 'lambda)))

(define (emit-immediate x)
  (format"    movl $~s, %eax\n" x))

(define (emit-if si env test case1 case2)
  (let ([alt-label (unique-label)]
        [end-label (unique-label)])
    (string-append
     (emit-expr si env test)
     (format
"    cmp $~s, %al
    je ~a
" bool_f alt-label)
     (emit-expr si env case1)
     (format
"    jmp ~a
~a:
" end-label alt-label)
     (emit-expr si env case2)
     (format
"~a:
" end-label))))

(define (emit-and si env args)
  (let ([end-label (unique-label)])
    (string-append
     (apply string-append
            (map (lambda (x)
                   (string-append
                    (emit-expr si env x)
                    (format
"    cmp $~s, %al
    je ~a
" bool_f end-label))) args))
     (format
"~a:
" end-label))))

(define (emit-add si env arg1 arg2)
  (if (eq? compile-bits 32)
      (string-append
       (emit-expr si env arg1)
       (format "    movl %eax, ~s(%esp)\n" si)
       (emit-expr (- si wordsize) env arg2)
       (format "    addl ~s(%esp), %eax\n" si))
      (string-append
       (emit-expr si env arg1)
       (format "    movl %eax, ~s(%rsp)\n" si)
       (emit-expr (- si wordsize) env arg2)
       (format "    addl ~s(%rsp), %eax\n" si))))

(define (emit-sub si env arg1 arg2)
  (if (eq? compile-bits 32)
      (string-append
       (emit-expr si env arg1)
       (format "    movl %eax, ~s(%esp)\n" si)
       (emit-expr (- si wordsize) env arg2)
       (format "    subl ~s(%esp), %eax\n" si)
       "    imul $-1, %eax\n")
      (string-append
       (emit-expr si env arg1)
       (format "    movl %eax, ~s(%rsp)\n" si)
       (emit-expr (- si wordsize) env arg2)
       (format "    subl ~s(%rsp), %eax\n" si)
       "    imul $-1, %eax\n")))

(define (emit-multiply si env arg1 arg2)
  (string-append
   (emit-expr si env arg1)
   (format "    movl %eax, ~s(%esp)\n" si)
   (emit-expr (- si wordsize) env arg2)
   (format "    imul ~s(%esp), %eax\n" si)
   "    movl $4, %ebx\n"
   "    div %ebx\n"))

;; (define (emit-let si env var-list body)
;;   (let ((curr-si si) (new-env nil))
;;     (apply string-append
;; 	   (map (lambda (x)
;; 		  (string-append
;; 		   (emit-expr (- curr-si wordsize) env (cadr x))
;; 		   (setq new-env (cons (list (car x) curr-si)))
;; 		   (format "movl %eax, ~s(%esp)\n" curr-si)))))))

(define (emit-let si env var-list body)
  (let ((curr-si si) (new-env env) 
	(reuse-si (- si (* wordsize (length var-list)))))
    (string-append
     (apply string-append
	    (map (lambda (x)
		   (let ((return-val
			  (string-append
			   (emit-expr reuse-si env (cadr x))
			   (if (eq? compile-bits 32)
			       (format "    movl %eax, ~s(%esp)\n" curr-si)
			       (format "    movq %rax, ~s(%rsp)\n" curr-si)))))
		     (set! new-env (cons (list (car x) curr-si) new-env))
		     (set! curr-si (- curr-si wordsize))
		     return-val))
		 var-list))
     (apply string-append
	    (map (lambda (x)
		   (emit-expr reuse-si new-env x))
		 body)))))

(define (next-sixteen num)
  (if (= (remainder num 16) 0)
      num
      (next-sixteen (- num wordsize))))

(define (sixteen-si-offset si args)
  (let ((no-offset-val (+ si (- (* args wordsize)))))
    (- (abs (- no-offset-val (next-sixteen no-offset-val))))))

(define (emit-apply si env function args)
    (let ((curr-si (+ si (sixteen-si-offset si (length args)))))
    (string-append
     (apply string-append
	    (map (lambda (x)
		   (set! curr-si (- curr-si wordsize))
		   (string-append
		    (emit-expr curr-si env x)
		    (format "    movl %eax, ~s(%esp)\n" curr-si)))
		 args))
     (emit-expr curr-si env function)
     (format "    addl $~s, %esp\n" curr-si)
     (format "    call *%eax\n")
     (format "    addl $~s, %esp\n" (- curr-si)))))

(define (emit-lambda args body)
    (let ((env '()) (si 0))
    (for-each (lambda (x)
		(set! si (+ si wordsize))
		(set! env (cons (list x si) env)))
	      (reverse args))
    (let ((lambda-name (unique-label)))
      (update-outputed-procedures
	(string-append
	 (format "~a:\n" lambda-name)
	 (apply string-append
		(map (lambda (x)
		       (emit-expr (- wordsize) env x))
		     body))
	 "    ret\n"))
      (format "    movl $~a, %eax\n" lambda-name))))

(define (emit-not)
  (format "    xorl $~s, %eax\n" bool_bit))

(define (emit-add1)
  (format "    addl $~s, %eax\n" (to_fixnum 1)))

(define (emit-sub1)
  (format "    subl $~s, %eax\n" (to_fixnum 1)))

(define (emit-is-zero)
  (format
"    cmp $~s, %eax
    je 1f
    movl $~s, %eax
    jmp 2f
1:
    movl $~s, %eax
2:
" (to_fixnum 0) bool_f bool_t))

(define (emit-is-null)
  (format
"    cmp $~s, %eax
    je 1f
    movl $~s, %eax
    jmp 2f
1:
    movl $~s, %eax
2:
" nil_t bool_f bool_t))

(define (emit-is-bool)
  (format
"    cmp $~s, %eax
    je 1f
    cmp $~s, %eax
    je 1f
    movl $~s, %eax
    jmp 2f
1:
    movl $~s, %eax
2:
" bool_t bool_f bool_f bool_t))

(define (emit-is-char)
  (format
"    cmp $~s, %al
    sete %al
    movzbl %al, %eax
    sal $~s, %al
    or $~s, %al
" char_tag bool_bit_pos bool_f))

(define (emit-is-fixnum)
  (format
"   and $~s, %al
   cmp $~s, %al
   sete %al
   movzbl %al, %eax
   sal $~s, %al
   or $~s, %al
" fixnum_tag_mask fixnum_tag bool_bit_pos bool_f))

(define (emit-primcall si env x args)
  (string-append
   (emit-expr env si args)
   (cond
     [(eq? x 'not)   (emit-not)]
     [(eq? x 'add1)  (emit-add1)]
     [(eq? x 'sub1)  (emit-sub1)]
     [(eq? x 'zero?) (emit-is-zero)]
     [(eq? x 'bool?) (emit-is-bool)]
     [(eq? x 'null?) (emit-is-null)]
     [(eq? x 'char?) (emit-is-char)]
     [(eq? x 'fixnum?) (emit-is-fixnum)])))

(define (emit-binary-prim si env x arg1 arg2)
  (string-append
   (cond
     [(eq? x '+) (emit-add si env arg1 arg2)]
     [(eq? x '-) (emit-sub si env arg1 arg2)]
     [(eq? x '*) (emit-multiply env si arg1 arg2)])))

(define (check-variable var env)
  (if (eq? var (car (car env)))
      (cadr (car env))
      (check-variable var (cdr env))))

(define (emit-variable env expr)
  ;; (print expr)
  ;; (print env)
  ;; (printf "\n")
  (if (eq? compile-bits 32)
      (format "    movl ~s(%esp), %eax\n"
	      (check-variable expr env))
      (format "    movq ~s(%rsp), %rax\n"
	      (check-variable expr env))))

(define (emit-expr si env expr)
  (cond
    [(immediate? expr) (emit-immediate expr)]
    [(if? expr) (emit-if si env (cadr expr) (caddr expr) (cadddr expr))]
    [(and? expr) (emit-and si env (cdr expr))]
    [(let? expr) (emit-let si env (cadr expr) (cddr expr))]
    [(primcall? expr) (emit-primcall si env (car expr) (cadr expr))]
    [(binary-prim? expr) (emit-binary-prim si env (car expr) (cadr expr) (caddr expr))]
    [(apply? expr) (emit-apply si env (cadr expr) (cddr expr))]
    [(lambda? expr) (emit-lambda (cadr expr) (cddr expr))]
    [else (emit-variable env expr)]))

(define (emit-start-32)
  (format
"    .globl scheme_entry
    .type scheme_entry, @function
scheme_entry:
    movl %esp, %ecx
    movl 4(%esp), %esp
    movl %ecx, -8(%esp)
    addl $-16, %esp
    call L_scheme_entry
    addl $16, %esp
    movl -8(%esp), %esp
    ret
"))

(define (emit-start-64)
  (format
"    .globl scheme_entry
    .type scheme_entry, @function
scheme_entry:
    movq %rsp, %rcx
    movq 8(%rsp), %rsp
    movq %rcx, -16(%rsp)
    addq $-16, %rsp
    call L_scheme_entry
    addq $16, %rsp
    movq -16(%rsp), %rsp
    ret
"))

(define (emit-program content)
  (with-output-to-file
    "output.s"
    (lambda ()
      (display
       (string-append
        "    .text\nL_scheme_entry:\n"
        (emit-expr (- wordsize) #t content)
        "    ret\n"
        (if (eq? compile-bits 32)
            (emit-start-32)
            (if (eq? compile-bits 64)
                (emit-start-64)
                "error"))
	outputed-procedures)))

    #:exists 'replace))

;; (print (emit-expr `(not ,bool_t)))
;(emit-program `(- ,(to_fixnum 54) (+ ,(to_fixnum 39) (+ ,(to_fixnum 1) ,(to_fixnum 4)))))

(emit-program nil_t)

;; (emit-program `(let ((fun (lambda (x) (+ x ,(to_fixnum 7)))))
;; 		 (apply fun ,(to_fixnum 3))))

;; (print (emit-program `(apply + ,(to_fixnum 3) ,(to_fixnum 4) (apply - ,bool_t ,bool_f))))
