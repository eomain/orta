
; programs main function
; declare void @main()

; terminate program by default
declare void @exit(i32)

; write bytes to stdout
declare i32 @puts(i8*)

; write bytes to stdout
declare i32 @printf(i8*, ...)

define void @print(i8* %s) {
    call i32 @puts(i8* %s)
    ret void
}

@.ifmt = constant [4 x i8] c"%ld\00"

define void @iprint(i64 %i) {
    %1 = getelementptr [4 x i8], [4 x i8]* @.ifmt, i32 0, i32 0
    call i32 (i8*, ...) @printf(i8* %1, i64 %i)
    ret void
}

; program entry point
define void @_start() {
    call void @main()
    call void @exit(i32 0)
    ret void
}
