#lang racket/base

(require racket/port)
(require net/http-client)
(require json)

(require "tools.rkt")

(define (port->string/utf-8 port)
  (bytes->string/utf-8 (port->bytes port)))

(define (port->string/gbk port)
  (define convert (bytes-open-converter "GBK" "UTF-8"))
  (let-values ([(result-bytes n-bytes stats) (bytes-convert convert (port->bytes port))])
    (bytes-close-converter convert)
    (bytes->string/utf-8 result-bytes)))

(define openai-base "api.aigcbest.top")
(define openai-uri "/v1/chat/completions")
(define api-key "hidden")
(define model "gpt-4-0125-preview")
(define python-path "C:/Users/neter/miniconda3/envs/pydata/python.exe")
(define mma-path "C:/Program Files/Wolfram Research/Mathematica/14.0/wolframscript.exe")

(define (send-openai-request model messages #:tools tools)
  (define data 
    (jsexpr->bytes
     `#hasheq((model . ,model)
              (messages . ,messages)
              (tools . ,tools))))
  
  (define-values (status response body)
    (http-sendrecv openai-base openai-uri
                   #:data data
                   #:method "POST"
                   #:ssl? 'tls13
                   #:headers `("Content-Type: application/json"
                               ,(format "Authorization: Bearer ~a" api-key))))
  
  (define js-result (string->jsexpr (port->string/utf-8 body)))
  
  js-result)


(define powershell-exec
  (procedure/llm "powershell" "Executes a Windows powershell command."
                 ([cmd "string" "powershell command string"])

                 (define-values (sub-proc out in err)
                   (subprocess #f #f #f (find-executable-path "powershell.exe") "-Command" cmd))
                 
                 (subprocess-wait sub-proc)

                 (define stdout (port->string/gbk out))
                 (define stderr (port->string/gbk err))
                 
                 (close-input-port out)
                 (close-output-port in)
                 (close-input-port err)
                 
                 (format "stdout=~a \n stderr=~a" stdout stderr)))

(define python-exec
  (procedure/llm "python" "Execute a Python code snippet. Make sure to print if you need a result."
                 ([code "string" "python code string."])

                 (define-values (sub-proc out in err)
                   (subprocess #f #f #f python-path "-c" code))

                 (subprocess-wait sub-proc)

                 (define stdout (port->string out))
                 (define stderr (port->string err))
                 
                 (close-input-port out)
                 (close-output-port in)
                 (close-input-port err)
                 
                 (format "stdout=~a \n stderr=~a" stdout stderr)))

(define mathematica-exec
  (procedure/llm "mathematica" "Execute a mathematica (wolframscript) code snippet."
                 ([code "string" "mathematica (wolframscript) code."])

                 (define-values (sub-proc out in err)
                   (subprocess #f #f #f mma-path "-code" code))

                 (subprocess-wait sub-proc)

                 (define stdout (port->string out))
                 (define stderr (port->string err))
                 
                 (close-input-port out)
                 (close-output-port in)
                 (close-input-port err)
                 
                 (format "stdout=~a \n stderr=~a" stdout stderr)))

(define f-list (list mathematica-exec))

(define openai-tools
  (map f-with-meta->openai-tool f-list))

(define (q messages)
  (define resp (send-openai-request model messages #:tools openai-tools))
  (define first-choice (car (hash-ref resp 'choices)))
  (define resp-message (hash-ref first-choice 'message))
  (define toolcall? (hash-has-key? resp-message 'tool_calls))

  (displayln (hash-ref resp-message 'content))

  (if toolcall?
      (let* ([openai-calls (hash-ref resp-message 'tool_calls)]
             
             [calls-result (map (λ (toolcall) (perform-openai-call
                                               f-list
                                               (hash-ref toolcall 'function)))
                                openai-calls)]
             
             [result-messages (map (λ (call-result call-id)
                                     `#hash((role . "tool")
                                            (content . ,call-result)
                                            (tool_call_id . ,call-id)))
                                   calls-result
                                   (map (λ (toolcall) (hash-ref toolcall 'id))
                                        openai-calls))]

             [message-to-send (append messages (list resp-message) result-messages)])

        (q message-to-send))

      (append messages (list resp-message))))

      
(define messages '())

(define (ask msg)
  (set! messages (append messages `(#hash((role . "user")
                                          (content . ,msg)))))
  (set! messages (q messages)))

(define (clean) (set! messages '()))