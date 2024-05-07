;;! target = "s390x"
;;!
;;! settings = ['enable_heap_access_spectre_mitigation=true']
;;!
;;! compile = true
;;!
;;! [globals.vmctx]
;;! type = "i64"
;;! vmctx = true
;;!
;;! [globals.heap_base]
;;! type = "i64"
;;! load = { base = "vmctx", offset = 0, readonly = true }
;;!
;;! [globals.heap_bound]
;;! type = "i64"
;;! load = { base = "vmctx", offset = 8, readonly = true }
;;!
;;! [[heaps]]
;;! base = "heap_base"
;;! min_size = 0x10000
;;! offset_guard_size = 0
;;! index_type = "i32"
;;! style = { kind = "dynamic", bound = "heap_bound" }

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!! GENERATED BY 'make-load-store-tests.sh' DO NOT EDIT !!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(module
  (memory i32 1)

  (func (export "do_store") (param i32 i32)
    local.get 0
    local.get 1
    i32.store8 offset=0xffff0000)

  (func (export "do_load") (param i32) (result i32)
    local.get 0
    i32.load8_u offset=0xffff0000))

;; function u0:0:
;;   unwind DefineNewFrame { offset_upward_to_caller_sp: 160, offset_downward_to_clobbers: 0 }
;;   stmg %r10, %r15, 80(%r15)
;;   unwind SaveReg { clobber_offset: 80, reg: p10i }
;;   unwind SaveReg { clobber_offset: 88, reg: p11i }
;;   unwind SaveReg { clobber_offset: 96, reg: p12i }
;;   unwind SaveReg { clobber_offset: 104, reg: p13i }
;;   unwind SaveReg { clobber_offset: 112, reg: p14i }
;;   unwind SaveReg { clobber_offset: 120, reg: p15i }
;;   unwind StackAlloc { size: 0 }
;; block0:
;;   llgfr %r14, %r2
;;   llilf %r5, 4294901761
;;   algfr %r5, %r2
;;   jgnle .+2 # trap=heap_oob
;;   lg %r10, 8(%r4)
;;   ag %r14, 0(%r4)
;;   llilh %r4, 65535
;;   agrk %r2, %r14, %r4
;;   lghi %r4, 0
;;   clgr %r5, %r10
;;   locgrh %r2, %r4
;;   stc %r3, 0(%r2)
;;   jg label1
;; block1:
;;   lmg %r10, %r15, 80(%r15)
;;   br %r14
;;
;; function u0:1:
;;   unwind DefineNewFrame { offset_upward_to_caller_sp: 160, offset_downward_to_clobbers: 0 }
;;   stmg %r14, %r15, 112(%r15)
;;   unwind SaveReg { clobber_offset: 112, reg: p14i }
;;   unwind SaveReg { clobber_offset: 120, reg: p15i }
;;   unwind StackAlloc { size: 0 }
;; block0:
;;   llgfr %r14, %r2
;;   llilf %r5, 4294901761
;;   algfr %r5, %r2
;;   jgnle .+2 # trap=heap_oob
;;   lg %r4, 8(%r3)
;;   ag %r14, 0(%r3)
;;   llilh %r3, 65535
;;   agrk %r2, %r14, %r3
;;   lghi %r3, 0
;;   clgr %r5, %r4
;;   locgrh %r2, %r3
;;   llc %r2, 0(%r2)
;;   jg label1
;; block1:
;;   lmg %r14, %r15, 112(%r15)
;;   br %r14