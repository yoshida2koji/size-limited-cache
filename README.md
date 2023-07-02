# size-limited-cache
memory cache that if the maximum size is exceeded when adding a cache,
the cache is cleared in order of oldest to newest time of last use until the cache size reaches a certain size.

## Usage
```lisp
;; 
(let ((manager (create-cache-manager :max-size 100)))
  (list (get-cache manager (list "aaa" 10))
        (add-cache manager (list "aaa" 10) "xxx" 10)
        (get-cache manager (list "aaa" 10))))
=> (NIL "xxx" "xxx")
```
