;;! target = "aarch64"
;;! test = "winch"

(module
    (func (param f32) (param f32) (result i32)
        (local.get 0)
        (local.get 1)
        (f32.ge)
    )
)
;; wasm[0]::function[0]:
;;       stp     x29, x30, [sp, #-0x10]!
;;       mov     x29, sp
;;       mov     x28, sp
;;       mov     x9, x0
;;       sub     sp, sp, #0x18
;;       mov     x28, sp
;;       stur    x0, [x28, #0x10]
;;       stur    x1, [x28, #8]
;;       stur    w0, [x28, #4]
;;       stur    w1, [x28]
;;       ldur    w0, [x28]
;;       ldur    w1, [x28, #4]
;;       fcmp    s0, s1
;;       cset    x0, ge
;;       add     sp, sp, #0x18
;;       mov     x28, sp
;;       ldp     x29, x30, [sp], #0x10
;;       ret
