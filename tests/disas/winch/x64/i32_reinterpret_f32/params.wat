;;! target = "x86_64"
;;! test = "winch"

(module
    (func (param f32) (result i32)
        (local.get 0)
        (i32.reinterpret_f32)
    )
)
;; wasm[0]::function[0]:
;;       pushq   %rbp
;;       movq    %rsp, %rbp
;;       movq    8(%rdi), %r11
;;       movq    (%r11), %r11
;;       addq    $0x20, %r11
;;       cmpq    %rsp, %r11
;;       ja      0x42
;;   1b: movq    %rdi, %r14
;;       subq    $0x20, %rsp
;;       movq    %rdi, 0x18(%rsp)
;;       movq    %rsi, 0x10(%rsp)
;;       movss   %xmm0, 0xc(%rsp)
;;       movss   0xc(%rsp), %xmm0
;;       movd    %xmm0, %eax
;;       addq    $0x20, %rsp
;;       popq    %rbp
;;       retq
;;   42: ud2
