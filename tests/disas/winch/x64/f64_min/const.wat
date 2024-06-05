;;! target = "x86_64"
;;! test = "winch"

(module
    (func (result f64)
        (f64.const 1.1)
        (f64.const 2.2)
        (f64.min)
    )
)
;; wasm[0]::function[0]:
;;       pushq   %rbp
;;       movq    %rsp, %rbp
;;       movq    8(%rdi), %r11
;;       movq    (%r11), %r11
;;       addq    $0x10, %r11
;;       cmpq    %rsp, %r11
;;       ja      0x6c
;;   1b: movq    %rdi, %r14
;;       subq    $0x10, %rsp
;;       movq    %rdi, 8(%rsp)
;;       movq    %rsi, (%rsp)
;;       movsd   0x3d(%rip), %xmm0
;;       movsd   0x3d(%rip), %xmm1
;;       ucomisd %xmm0, %xmm1
;;       jne     0x5e
;;       jp      0x54
;;   4b: orpd    %xmm0, %xmm1
;;       jmp     0x62
;;   54: addsd   %xmm0, %xmm1
;;       jp      0x62
;;   5e: minsd   %xmm0, %xmm1
;;       movapd  %xmm1, %xmm0
;;       addq    $0x10, %rsp
;;       popq    %rbp
;;       retq
;;   6c: ud2
;;   6e: addb    %al, (%rax)
