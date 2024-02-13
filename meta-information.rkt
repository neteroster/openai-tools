#lang racket/base

(provide (struct-out v-meta-struct)
         (struct-out f-with-meta-struct))

(struct v-meta-struct (id jstype desc))
(struct f-with-meta-struct (id desc v-meta f))

