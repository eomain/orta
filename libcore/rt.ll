
; programs main function
; declare void @main()

; terminate program by default
declare void @exit(i32)

; write bytes to stdout
declare i32 @puts(i8*)

define void @print(i8* %s) {
    call i32 @puts(i8* %s)
    ret void
}

; program entry point
define void @_start() {
    call void @main()
    call void @exit(i32 0)
    ret void
}
