#lang racket/base

(require json)

(require (for-syntax "meta-infomation.rkt")
         "meta-infomation.rkt")

(provide procedure/llm
         f-with-meta->openai-tool
         perform-openai-call)

(define-syntax-rule
  (procedure/llm f-id f-desc ([v-id v-jstype v-desc] ...) f-body ...)
  (let* ([f (λ (v-id ...) f-body ...)]
         [v-meta (list (v-meta-struct 'v-id v-jstype v-desc) ...)]
         [f-with-meta (f-with-meta-struct f-id f-desc v-meta f)])
    f-with-meta))

(define (f-with-meta->openai-tool f-with-meta)
  (define v-meta (f-with-meta-struct-v-meta f-with-meta))
  
  (let* ([openai-properties
          (make-immutable-hash
           (map (λ (param)
                  (cons (v-meta-struct-id param) `#hash((type . ,(v-meta-struct-jstype param))
                                                        (description . ,(v-meta-struct-desc param)))))
                v-meta))]
         
         [openai-paramters `#hash((type . "object") (properties . ,openai-properties))]
         
         [openai-function
          `#hash((name . ,(f-with-meta-struct-id f-with-meta))
                 (description . ,(f-with-meta-struct-desc f-with-meta))
                 (parameters . ,openai-paramters))]
         
         [openai-tools `#hash((type . "function")
                              (function . ,openai-function))])
    
    openai-tools))

(define (perform-openai-call f-list f-call)
  (define f-id (hash-ref f-call 'name))
  (define raw-f-param (string->jsexpr (hash-ref f-call 'arguments)))
  
  (define targetf-info
    (findf (λ (x) (equal? (f-with-meta-struct-id x) f-id)) f-list))

  (define v-meta (f-with-meta-struct-v-meta targetf-info))
  
  (define f-param
    (map (λ (v-id) (hash-ref raw-f-param v-id))
         (map v-meta-struct-id v-meta)))

  (define call-result (apply (f-with-meta-struct-f targetf-info) f-param))

  (displayln (format "[DEBUG] Function called: ~a, param: ~a \n result: ~a" f-id f-param call-result))
  
  call-result)

;; End Tool Ralated

