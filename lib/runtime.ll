; ModuleID = 'lib/runtime.c'
source_filename = "lib/runtime.c"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx12.0.0"

%struct.__sFILE = type { i8*, i32, i32, i16, i16, %struct.__sbuf, i32, i8*, i32 (i8*)*, i32 (i8*, i8*, i32)*, i64 (i8*, i64, i32)*, i32 (i8*, i8*, i32)*, %struct.__sbuf, %struct.__sFILEX*, i32, [3 x i8], [1 x i8], %struct.__sbuf, i32, i64 }
%struct.__sFILEX = type opaque
%struct.__sbuf = type { i8*, i32 }

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.str.1 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@__stdinp = external global %struct.__sFILE*, align 8
@.str.3 = private unnamed_addr constant [23 x i8] c"readInt getline error\0A\00", align 1
@.str.4 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str.5 = private unnamed_addr constant [26 x i8] c"readString getline error\0A\00", align 1
@.str.6 = private unnamed_addr constant [15 x i8] c"runtime error\0A\00", align 1

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @printInt(i32 %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i64 0, i64 0), i32 %3)
  ret void
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @printString(i8* %0) #0 {
  %2 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %3 = load i8*, i8** %2, align 8
  %4 = icmp eq i8* %3, null
  br i1 %4, label %5, label %7

5:                                                ; preds = %1
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.1, i64 0, i64 0))
  br label %10

7:                                                ; preds = %1
  %8 = load i8*, i8** %2, align 8
  %9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.2, i64 0, i64 0), i8* %8)
  br label %10

10:                                               ; preds = %7, %5
  ret void
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @readInt() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8*, align 8
  %3 = alloca i64, align 8
  store i8* null, i8** %2, align 8
  store i64 0, i64* %3, align 8
  %4 = load %struct.__sFILE*, %struct.__sFILE** @__stdinp, align 8
  %5 = call i64 @getline(i8** %2, i64* %3, %struct.__sFILE* %4)
  %6 = icmp eq i64 %5, -1
  br i1 %6, label %7, label %9

7:                                                ; preds = %0
  %8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([23 x i8], [23 x i8]* @.str.3, i64 0, i64 0))
  call void @exit(i32 1) #6
  unreachable

9:                                                ; preds = %0
  %10 = load i8*, i8** %2, align 8
  %11 = call i32 (i8*, i8*, ...) @sscanf(i8* %10, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.4, i64 0, i64 0), i32* %1)
  %12 = load i32, i32* %1, align 4
  ret i32 %12
}

declare i64 @getline(i8**, i64*, %struct.__sFILE*) #1

; Function Attrs: noreturn
declare void @exit(i32) #2

declare i32 @sscanf(i8*, i8*, ...) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define i8* @readString() #0 {
  %1 = alloca i8*, align 8
  %2 = alloca i64, align 8
  store i8* null, i8** %1, align 8
  store i64 0, i64* %2, align 8
  %3 = load %struct.__sFILE*, %struct.__sFILE** @__stdinp, align 8
  %4 = call i64 @getline(i8** %1, i64* %2, %struct.__sFILE* %3)
  %5 = icmp eq i64 %4, -1
  br i1 %5, label %6, label %8

6:                                                ; preds = %0
  %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #6
  unreachable

8:                                                ; preds = %0
  %9 = load i8*, i8** %1, align 8
  %10 = call i64 @strlen(i8* %9)
  store i64 %10, i64* %2, align 8
  %11 = load i8*, i8** %1, align 8
  %12 = load i64, i64* %2, align 8
  %13 = sub i64 %12, 1
  %14 = getelementptr inbounds i8, i8* %11, i64 %13
  %15 = load i8, i8* %14, align 1
  %16 = sext i8 %15 to i32
  %17 = icmp eq i32 %16, 10
  br i1 %17, label %18, label %23

18:                                               ; preds = %8
  %19 = load i8*, i8** %1, align 8
  %20 = load i64, i64* %2, align 8
  %21 = sub i64 %20, 1
  %22 = getelementptr inbounds i8, i8* %19, i64 %21
  store i8 0, i8* %22, align 1
  br label %23

23:                                               ; preds = %18, %8
  %24 = load i8*, i8** %1, align 8
  ret i8* %24
}

declare i64 @strlen(i8*) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @error() #0 {
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.6, i64 0, i64 0))
  call void @exit(i32 1) #6
  unreachable
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i8* @concatStrings(i8* %0, i8* %1) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca i8*, align 8
  store i8* %0, i8** %4, align 8
  store i8* %1, i8** %5, align 8
  %9 = load i8*, i8** %4, align 8
  %10 = icmp eq i8* %9, null
  br i1 %10, label %11, label %13

11:                                               ; preds = %2
  %12 = load i8*, i8** %5, align 8
  store i8* %12, i8** %3, align 8
  br label %50

13:                                               ; preds = %2
  %14 = load i8*, i8** %5, align 8
  %15 = icmp eq i8* %14, null
  br i1 %15, label %16, label %18

16:                                               ; preds = %13
  %17 = load i8*, i8** %4, align 8
  store i8* %17, i8** %3, align 8
  br label %50

18:                                               ; preds = %13
  %19 = load i8*, i8** %4, align 8
  %20 = call i64 @strlen(i8* %19)
  store i64 %20, i64* %6, align 8
  %21 = load i8*, i8** %5, align 8
  %22 = call i64 @strlen(i8* %21)
  store i64 %22, i64* %7, align 8
  %23 = load i64, i64* %6, align 8
  %24 = load i64, i64* %7, align 8
  %25 = add i64 %23, %24
  %26 = add i64 %25, 1
  %27 = call align 16 i8* @malloc(i64 %26) #7
  store i8* %27, i8** %8, align 8
  %28 = load i8*, i8** %8, align 8
  %29 = load i8*, i8** %4, align 8
  %30 = load i64, i64* %6, align 8
  %31 = load i8*, i8** %8, align 8
  %32 = call i64 @llvm.objectsize.i64.p0i8(i8* %31, i1 false, i1 true, i1 false)
  %33 = call i8* @__memcpy_chk(i8* %28, i8* %29, i64 %30, i64 %32) #8
  %34 = load i8*, i8** %8, align 8
  %35 = load i64, i64* %6, align 8
  %36 = getelementptr inbounds i8, i8* %34, i64 %35
  %37 = load i8*, i8** %5, align 8
  %38 = load i64, i64* %7, align 8
  %39 = load i8*, i8** %8, align 8
  %40 = load i64, i64* %6, align 8
  %41 = getelementptr inbounds i8, i8* %39, i64 %40
  %42 = call i64 @llvm.objectsize.i64.p0i8(i8* %41, i1 false, i1 true, i1 false)
  %43 = call i8* @__memcpy_chk(i8* %36, i8* %37, i64 %38, i64 %42) #8
  %44 = load i8*, i8** %8, align 8
  %45 = load i64, i64* %6, align 8
  %46 = load i64, i64* %7, align 8
  %47 = add i64 %45, %46
  %48 = getelementptr inbounds i8, i8* %44, i64 %47
  store i8 0, i8* %48, align 1
  %49 = load i8*, i8** %8, align 8
  store i8* %49, i8** %3, align 8
  br label %50

50:                                               ; preds = %18, %16, %11
  %51 = load i8*, i8** %3, align 8
  ret i8* %51
}

; Function Attrs: allocsize(0)
declare align 16 i8* @malloc(i64) #3

; Function Attrs: nounwind
declare i8* @__memcpy_chk(i8*, i8*, i64, i64) #4

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.objectsize.i64.p0i8(i8*, i1 immarg, i1 immarg, i1 immarg) #5

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @equStrings(i8* %0, i8* %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  store i8* %0, i8** %4, align 8
  store i8* %1, i8** %5, align 8
  %6 = load i8*, i8** %4, align 8
  %7 = icmp eq i8* %6, null
  br i1 %7, label %8, label %11

8:                                                ; preds = %2
  %9 = load i8*, i8** %5, align 8
  %10 = icmp ne i8* %9, null
  br i1 %10, label %17, label %11

11:                                               ; preds = %8, %2
  %12 = load i8*, i8** %4, align 8
  %13 = icmp ne i8* %12, null
  br i1 %13, label %14, label %18

14:                                               ; preds = %11
  %15 = load i8*, i8** %5, align 8
  %16 = icmp eq i8* %15, null
  br i1 %16, label %17, label %18

17:                                               ; preds = %14, %8
  store i32 0, i32* %3, align 4
  br label %32

18:                                               ; preds = %14, %11
  %19 = load i8*, i8** %4, align 8
  %20 = icmp eq i8* %19, null
  br i1 %20, label %21, label %25

21:                                               ; preds = %18
  %22 = load i8*, i8** %5, align 8
  %23 = icmp eq i8* %22, null
  br i1 %23, label %24, label %25

24:                                               ; preds = %21
  store i32 1, i32* %3, align 4
  br label %32

25:                                               ; preds = %21, %18
  %26 = load i8*, i8** %4, align 8
  %27 = load i8*, i8** %5, align 8
  %28 = call i32 @strcmp(i8* %26, i8* %27)
  %29 = icmp ne i32 %28, 0
  %30 = xor i1 %29, true
  %31 = zext i1 %30 to i32
  store i32 %31, i32* %3, align 4
  br label %32

32:                                               ; preds = %25, %24, %17
  %33 = load i32, i32* %3, align 4
  ret i32 %33
}

declare i32 @strcmp(i8*, i8*) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @neStrings(i8* %0, i8* %1) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  store i8* %0, i8** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load i8*, i8** %3, align 8
  %6 = load i8*, i8** %4, align 8
  %7 = call i32 @equStrings(i8* %5, i8* %6)
  %8 = icmp ne i32 %7, 0
  %9 = xor i1 %8, true
  %10 = zext i1 %9 to i32
  ret i32 %10
}

attributes #0 = { noinline nounwind optnone ssp uwtable "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+v8.5a,+zcm,+zcz" }
attributes #1 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+v8.5a,+zcm,+zcz" }
attributes #2 = { noreturn "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+v8.5a,+zcm,+zcz" }
attributes #3 = { allocsize(0) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+v8.5a,+zcm,+zcz" }
attributes #4 = { nounwind "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+v8.5a,+zcm,+zcz" }
attributes #5 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #6 = { noreturn }
attributes #7 = { allocsize(0) }
attributes #8 = { nounwind }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7}
!llvm.ident = !{!8}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 1, !"branch-target-enforcement", i32 0}
!2 = !{i32 1, !"sign-return-address", i32 0}
!3 = !{i32 1, !"sign-return-address-all", i32 0}
!4 = !{i32 1, !"sign-return-address-with-bkey", i32 0}
!5 = !{i32 7, !"PIC Level", i32 2}
!6 = !{i32 7, !"uwtable", i32 1}
!7 = !{i32 7, !"frame-pointer", i32 1}
!8 = !{!"Homebrew clang version 13.0.0"}
