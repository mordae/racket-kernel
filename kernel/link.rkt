#lang racket/base
;
; Network Interfaces
;

(require racket/contract
         racket/list)

(require misc1/throw
         rtnl)

(require "private/socket.rkt"
         "private/ffi.rkt"
         "private/sysfs-net.rkt")

(provide
  (all-from-out "private/ffi.rkt"))

(provide
  (contract-out
    (link-index? predicate/c)
    (link? predicate/c)
    (vlan? predicate/c)
    (bond? predicate/c)
    (bridge? predicate/c)
    (links (-> (listof link-index?)))
    (link-family (-> link-index? symbol?))
    (set-link-name! (-> link-index? (string-len/c 16) void?))
    (link-delete! (-> link-index? void?))
    (link-mtu (-> link-index? exact-nonnegative-integer?))
    (set-link-mtu! (-> link-index? exact-nonnegative-integer? void?))
    (link-bridge (-> link-index? maybe-link-index/c))
    (set-link-bridge! (-> link-index? maybe-link-index/c void?))
    (link-bond (-> link-index? maybe-link-index/c))
    (set-link-bond! (-> link-index? maybe-link-index/c void?))
    (link-flags (-> link-index? (listof symbol?)))
    (set-link-flags! change-link-flags-proc/c)
    (unset-link-flags! change-link-flags-proc/c)
    (link-address (-> link-index? string?))
    (set-link-address! (-> link-index? string? void?))
    (link-broadcast (-> link-index? string?))
    (set-link-broadcast! (-> link-index? string? void?))
    (link-carrier (-> link-index? (symbols 'up 'down)))
    (link-parent (-> link-index? maybe-link-index/c))
    (create-vlan (-> link-index? string? (integer-in 0 4095) void?))
    (vlan-flags (-> link-index? (listof symbol?)))
    (set-vlan-flags! change-link-flags-proc/c)
    (unset-vlan-flags! change-link-flags-proc/c)
    (create-bond (-> string? void?))
    (bond-slaves (-> link-index? (listof link-index?)))
    (bond-mode (-> link-index? symbol?))
    (set-bond-mode! (-> link-index? symbol? void?))
    (bond-downdelay (-> link-index? (or/c #f exact-nonnegative-integer?)))
    (set-bond-downdelay! (-> link-index? exact-nonnegative-integer? void?))
    (bond-updelay (-> link-index? (or/c #f exact-nonnegative-integer?)))
    (set-bond-updelay! (-> link-index? exact-nonnegative-integer? void?))
    (bond-miimon (-> link-index? (or/c #f exact-nonnegative-integer?)))
    (set-bond-miimon! (-> link-index? exact-nonnegative-integer? void?))
    (bond-lacp-rate (-> link-index? symbol?))
    (set-bond-lacp-rate! (-> link-index? symbol? void?))
    (bond-xmit-hash-policy (-> link-index? symbol?))
    (set-bond-xmit-hash-policy! (-> link-index? symbol? void?))
    (create-bridge (-> string? void?))
    (bridge-flags (-> link-index? (listof symbol?)))
    (set-bridge-flags! change-link-flags-proc/c)
    (unset-bridge-flags! change-link-flags-proc/c)
    (bridge-priority (-> link-index? exact-nonnegative-integer?))
    (set-bridge-priority! (-> link-index? exact-nonnegative-integer? void?))
    (bridge-cost (-> link-index? exact-nonnegative-integer?))
    (set-bridge-cost! (-> link-index? exact-nonnegative-integer? void?))))


(define link-index?
  exact-positive-integer?)

(define change-link-flags-proc/c
  (->* (link-index?) ()
       #:rest (listof (or/c symbol? (listof symbol?)))
       void?))

(define (link? index)
  (and (link-index? index)
       (link-index->name index)
       #t))

(define maybe-link-index/c
  (or/c #f link-index?))


(define (check-link owner index)
  (unless (link? index)
    (throw exn:fail owner "link does not exist" "ifindex" index)))

(define (check-vlan owner index)
  (unless (vlan? index)
    (throw exn:fail owner "vlan does not exist" "ifindex" index)))

(define (check-bond owner index)
  (unless (bond? index)
    (throw exn:fail owner "bond does not exist" "ifindex" index)))

(define (check-bridge owner index)
  (unless (bridge? index)
    (throw exn:fail owner "bridge does not exist" "ifindex" index)))


(define (index->link index)
  (rtnl-link-get/kernel socket index #f))


(define-syntax-rule (update-link (index new-link) body ...)
  (let ((old-link (index->link index))
        (new-link (rtnl-link-alloc)))
    (begin body ...)
    (rtnl-link-change! socket old-link new-link)))


(define (zero->false int)
  (if (= int 0) #f int))

(define (false->zero int-or-false)
  (if int-or-false int-or-false 0))


(define (links)
  (for/list ((link (nl-cache->list (rtnl-link-alloc-cache socket))))
    (rtnl-link-get-ifindex link)))

(define (link-family index)
  (check-link 'link-family index)
  (let ((link (index->link index)))
    (rtnl-link-get-family link)))

(define (set-link-name! index name)
  (check-link 'set-link-name! index)
  (update-link (index link)
    (rtnl-link-set-name! link name)))

(define (link-delete! index)
  (check-link 'link-delete! index)
  (let ((link (index->link index)))
    (rtnl-link-delete! socket link)))

(define (link-mtu index)
  (check-link 'link-mtu index)
  (let ((link (index->link index)))
    (rtnl-link-get-mtu link)))

(define (set-link-mtu! index mtu)
  (check-link 'set-link-mtu! index)
  (update-link (index link)
    (rtnl-link-set-mtu! link mtu)))

(define (link-bridge index)
  (check-link 'link-bridge index)
  (let ((link (index->link index)))
    (let ((master (rtnl-link-get-master link)))
      (zero->false master))))

(define (set-link-bridge! link-index bridge-index)
  (check-link 'set-link-bridge! link-index)
  (check-bridge 'set-link-bridge! bridge-index)
  (update-link (link-index link)
    (rtnl-link-set-master! link (false->zero bridge-index))))

(define (link-bond index)
  (check-link 'link-bond index)
  (link-bond/name (link-index->name index)))

(define (set-link-bond! link-index bond-index)
  (check-link 'set-link-bond! link-index)
  (check-bond 'set-link-bond! bond-index)
  (let ((old-bond-index (link-bond link-index)))
    (unless (equal? old-bond-index bond-index)
      (when old-bond-index
        (rtnl-link-bond-release/ifindex! socket link-index))
      (when bond-index
        (rtnl-link-bond-enslave/ifindex! socket bond-index link-index)))))

(define (link-flags index)
  (check-link 'link-flags index)
  (let ((link (index->link index)))
    (rtnl-link-get-flags link)))

(define (set-link-flags! index . flags)
  (check-link 'set-link-flags! index)
  (update-link (index link)
    (rtnl-link-set-flags! link (flatten flags))))

(define (unset-link-flags! index . flags)
  (check-link 'unset-link-flags! index)
  (update-link (index link)
    (rtnl-link-unset-flags! link (flatten flags))))

(define (link-address index)
  (check-link 'link-address index)
  (let ((link (index->link index)))
    (nl-addr->string (rtnl-link-get-addr link))))

(define (set-link-address! index address)
  (check-link 'set-link-address! index)
  (update-link (index link)
    (rtnl-link-set-addr! link (nl-addr-parse address 'unspec))))

(define (link-broadcast index)
  (check-link 'link-broadcast index)
  (let ((link (index->link index)))
    (nl-addr->string (rtnl-link-get-addr link))))

(define (set-link-broadcast! index address)
  (check-link 'set-link-broadcast! index)
  (update-link (index link)
    (rtnl-link-set-addr! link (nl-addr-parse address 'unspec))))

(define (link-carrier index)
  (check-link 'link-carrier index)
  (let ((link (index->link index)))
    (rtnl-link-get-carrier link)))

(define (link-parent index)
  (check-link 'link-parent index)
  (let ((link (index->link index)))
    (zero->false (rtnl-link-get-link link))))

(define (vlan? index)
  (and (link? index)
       (let ((link (index->link index)))
         (rtnl-link-is-vlan? link))))

(define (create-vlan parent-index name tag)
  (check-link 'create-vlan parent-index)
  (let ((vlan (rtnl-link-vlan-alloc)))
    (rtnl-link-set-link! vlan parent-index)
    (rtnl-link-vlan-set-id! vlan tag)
    (rtnl-link-set-name! vlan name)
    (rtnl-link-add! vlan)))

(define (vlan-flags index)
  (check-vlan 'vlan-flags index)
  (let ((link (index->link index)))
    (rtnl-link-vlan-get-flags link)))

(define (set-vlan-flags! index . flags)
  (check-vlan 'set-vlan-flags! index)
  (update-link (index link)
    (rtnl-link-vlan-set-flags! link (flatten flags))))

(define (unset-vlan-flags! index . flags)
  (check-vlan 'unset-vlan-flags! index)
  (update-link (index link)
    (rtnl-link-vlan-unset-flags! link (flatten flags))))

(define (create-bond name)
  (let ((opts (rtnl-link-bond-alloc)))
    (rtnl-link-bond-add! socket name opts)))

(define (bond-slaves index)
  (check-bond 'bond-slaves index)
  (map rtnl-link-get-ifindex
       (filter (λ (link)
                 (let ((name (rtnl-link-get-name link)))
                   (equal? index (link-bond/name name))))
               (nl-cache->list (rtnl-link-alloc-cache socket)))))

(define (bond? index)
  (and (link? index)
       (let ((link (index->link index)))
         (eq? 'bond (rtnl-link-get-type link)))))

(define (bond-mode index)
  (check-bond 'bond-mode index)
  (bond-mode/name (link-index->name index)))

(define (set-bond-mode! index mode)
  (check-bond 'set-bond-mode! index)
  (set-bond-mode/name! (link-index->name index) mode))

(define (bond-downdelay index)
  (check-bond 'bond-downdelay index)
  (bond-downdelay/name (link-index->name index)))

(define (set-bond-downdelay! index downdelay)
  (check-bond 'set-bond-downdelay! index)
  (set-bond-downdelay/name! (link-index->name index) downdelay))

(define (bond-updelay index)
  (check-bond 'bond-updelay index)
  (bond-updelay/name (link-index->name index)))

(define (set-bond-updelay! index updelay)
  (check-bond 'set-bond-updelay! index)
  (set-bond-updelay/name! (link-index->name index) updelay))

(define (bond-miimon index)
  (check-bond 'bond-miimon index)
  (bond-miimon/name (link-index->name index)))

(define (set-bond-miimon! index miimon)
  (check-bond 'set-bond-miimon! index)
  (set-bond-miimon/name! (link-index->name index) miimon))

(define (bond-lacp-rate index)
  (check-bond 'bond-lacp-rate index)
  (bond-lacp-rate/name (link-index->name index)))

(define (set-bond-lacp-rate! index lacp-rate)
  (check-bond 'set-bond-lacp-rate! index)
  (set-bond-lacp-rate/name! (link-index->name index) lacp-rate))

(define (bond-xmit-hash-policy index)
  (check-bond 'bond-xmit-hash-policy index)
  (bond-xmit-hash-policy/name (link-index->name index)))

(define (set-bond-xmit-hash-policy! index xmit-hash-policy)
  (check-bond 'set-bond-xmit-hash-policy! index)
  (set-bond-xmit-hash-policy/name! (link-index->name index) xmit-hash-policy))

(define (bridge? index)
  (and (link? index)
       (let ((link (index->link index)))
         (rtnl-link-is-bridge? link))))

(define (create-bridge name)
  (let ((link (rtnl-link-bridge-alloc)))
    (rtnl-link-bridge-add! socket name)))

(define (bridge-flags index)
  (check-bridge 'bridge-flags index)
  (let ((link (index->link index)))
    (rtnl-link-bridge-get-flags link)))

(define (set-bridge-flags! index . flags)
  (check-bridge 'set-bridge-flags! index)
  (update-link (index link)
    (rtnl-link-bridge-set-flags! link (flatten flags))))

(define (unset-bridge-flags! index . flags)
  (check-bridge 'unset-bridge-flags! index)
  (update-link (index link)
    (rtnl-link-bridge-unset-flags! link (flatten flags))))

(define (bridge-priority index)
  (check-bridge 'bridge-priority index)
  (let ((link (index->link index)))
    (rtnl-link-bridge-get-priority link)))

(define (set-bridge-priority! index priority)
  (check-bridge 'set-bridge-priority! index)
  (update-link (index link)
    (rtnl-link-bridge-set-priority! link priority)))

(define (bridge-cost index)
  (check-bridge 'bridge-cost index)
  (let ((link (index->link index)))
    (rtnl-link-bridge-get-cost link)))

(define (set-bridge-cost! index cost)
  (check-bridge 'set-bridge-cost! index)
  (update-link (index link)
    (rtnl-link-bridge-set-cost! link cost)))


; vim:set ts=2 sw=2 et:
