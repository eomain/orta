
; programs main function
declare void @main()

; terminate program by default
declare void @exit(i32)

; program entry point
define void @_start() {
    call void @main()
    call void @exit(i32 0)
    ret void
}
