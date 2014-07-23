#lang racket/base
;
; Network Interface Addresses
;

(require racket/contract
         racket/undefined
         racket/match)

(require rtnl)

(require "private/socket.rkt"
         "link.rkt")

(provide
  (contract-out
    (address? predicate/c)
    (address-string? predicate/c)
    (address (-> link-index? address-string? address?))
    (addresses (->* () (link-index?) (listof address?)))
    (address-link (-> address? link-index?))
    (address-local (-> address? address-string?))
    (address-peer (-> address? (or/c #f address-string?)))
    (address-broadcast (-> address? (or/c #f address-string?)))
    (address-multicast (-> address? (or/c #f address-string?)))
    (address-anycast (-> address? (or/c #f address-string?)))
    (address-scope (-> address? (integer-in 0 255)))
    (address-scope/name (-> address? symbol?))
    (address-flags (-> address? (listof symbol?)))

    (address-delete! (-> address? void?))
    (create-address (->* (link-index? address-string?)
                         (#:peer address-string?
                          #:broadcast address-string?
                          #:multicast address-string?
                          #:anycast address-string?)
                         address?))))


(struct address
  (link local)
  #:transparent)


(define (address-string? str)
  (and (string? str)
       (nl-addr-parse str 'unspec)
       #t))

(define (address->rtnl-addr a)
  (match-let (((address link local) a))
    (let ((local (nl-addr-parse local 'unspec)))
      (rtnl-addr-get (rtnl-addr-alloc-cache socket) link local))))


(define (create-address link local #:peer (peer undefined)
                                   #:broadcast (broadcast undefined)
                                   #:multicast (multicast undefined)
                                   #:anycast (anycast undefined))
  (let ((addr (rtnl-addr-alloc)))
    (rtnl-addr-set-ifindex! addr link)

    (let ((local (nl-addr-parse local 'unspec)))
      (rtnl-addr-set-local! addr local))

    (unless (eq? peer undefined)
      (let ((peer (nl-addr-parse peer 'unspec)))
        (rtnl-addr-set-peer! addr peer)))

    (unless (eq? broadcast undefined)
      (let ((broadcast (nl-addr-parse broadcast 'unspec)))
        (rtnl-addr-set-broadcast! addr broadcast)))

    (unless (eq? multicast undefined)
      (let ((multicast (nl-addr-parse multicast 'unspec)))
        (rtnl-addr-set-multicast! addr multicast)))

    (unless (eq? anycast undefined)
      (let ((anycast (nl-addr-parse anycast 'unspec)))
        (rtnl-addr-set-anycast! addr anycast)))

    (rtnl-addr-add! socket addr)
    (address link local)))

(define (address-delete! a)
  (match-let (((address link local) a))
    (let ((addr (rtnl-addr-alloc))
          (local (nl-addr-parse local 'unspec)))
      (rtnl-addr-set-ifindex! addr link)
      (rtnl-addr-set-local! addr local)
      (rtnl-addr-delete! socket addr))))

(define (addresses (link undefined))
  (if (eq? undefined link)
      (all-addresses)
      (filter (λ (a)
                (= link (address-link a)))
              (all-addresses))))

(define (all-addresses)
  (for/list ((addr (nl-cache->list (rtnl-addr-alloc-cache socket))))
    (address (rtnl-addr-get-ifindex addr)
             (nl-addr->string (rtnl-addr-get-local addr)))))

(define (address-peer address)
  (let ((addr (address->rtnl-addr address)))
    (let ((peer-addr (rtnl-addr-get-peer addr)))
      (and peer-addr (nl-addr->string peer-addr)))))

(define (address-broadcast address)
  (let ((addr (address->rtnl-addr address)))
    (let ((broadcast-addr (rtnl-addr-get-broadcast addr)))
      (and broadcast-addr (nl-addr->string broadcast-addr)))))

(define (address-multicast address)
  (let ((addr (address->rtnl-addr address)))
    (let ((multicast-addr (rtnl-addr-get-multicast addr)))
      (and multicast-addr (nl-addr->string multicast-addr)))))

(define (address-anycast address)
  (let ((addr (address->rtnl-addr address)))
    (let ((anycast-addr (rtnl-addr-get-anycast addr)))
      (and anycast-addr (nl-addr->string anycast-addr)))))

(define (address-scope address)
  (let ((addr (address->rtnl-addr address)))
    (rtnl-addr-get-scope addr)))

(define (address-scope/name address)
  (string->symbol
    (rtnl-scope2str (address-scope address))))

(define (address-flags address)
  (let ((addr (address->rtnl-addr address)))
    (rtnl-addr-get-flags addr)))


; vim:set ts=2 sw=2 et:
