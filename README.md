# carScheme

一个用Haskell实现的Scheme语言解释器。

## 构建

使用cabal构建项目：

```bash
cabal build
```

## 运行

### REPL交互模式

直接运行解释器进入交互式环境：

```bash
cabal run carScheme
```

或使用编译后的可执行文件：

```bash
./dist-newstyle/build/*/ghc-*/carScheme-*/x/carScheme/build/carScheme/carScheme
```

在REPL中输入`quit`退出。

### 脚本执行模式

执行Scheme脚本文件：

```bash
cabal run carScheme -- script.scm [args...]
```

脚本中可通过`args`变量访问命令行参数。

## 支持的特性

### 数据类型

- 整数：`42`, `#d10`, `#x1A`, `#o12`, `#b1010`
- 浮点数：`3.14`, `2.5e-3`
- 字符串：`"hello world"`
- 布尔值：`#t`, `#f`
- 字符：`#\a`, `#\space`, `#\newline`
- 符号：`symbol`, `+`, `list?`
- 列表：`(1 2 3)`, `'(a b c)`
- 点列表：`(1 2 . 3)`
- 向量：`#(1 2 3)`

### 特殊形式

- `quote` / `'`：引用
- `if`：条件判断
- `cond`：多分支条件
- `case`：模式匹配
- `define`：定义变量和函数
- `set!`：修改变量
- `lambda`：匿名函数

### 内置函数

#### 算术运算

```scheme
(+ 1 2 3)        ; 6
(- 10 3)         ; 7
(* 2 3 4)        ; 24
(/ 10 2)         ; 5
(mod 10 3)       ; 1
(quotient 10 3)  ; 3
(remainder 10 3) ; 1
```

#### 比较运算

```scheme
(= 1 1)          ; #t
(< 1 2)          ; #t
(> 3 2)          ; #t
(<= 1 1)         ; #t
(>= 2 1)         ; #t
```

#### 布尔运算

```scheme
(&& #t #t)       ; #t
(|| #f #t)       ; #t
```

#### 字符串操作

```scheme
(string=? "a" "a")   ; #t
(string<? "a" "b")   ; #t
```

#### 类型判断

```scheme
(symbol? 'x)     ; #t
(string? "hi")   ; #t
(number? 42)     ; #t
(bool? #t)       ; #t
(list? '(1 2))   ; #t
(pair? '(1 . 2)) ; #t
(vector? #(1 2)) ; #t
(char? #\a)      ; #t
(float? 3.14)    ; #t
```

#### 类型转换

```scheme
(symbol->string 'hello)  ; "hello"
(string->symbol "world") ; world
```

#### 列表操作

```scheme
(cons 1 '(2 3))  ; (1 2 3)
(car '(1 2 3))   ; 1
(cdr '(1 2 3))   ; (2 3)
```

#### 相等性判断

```scheme
(eqv? 1 1)       ; #t
(eqv? '(1 2) '(1 2)) ; #t
```

#### 文件I/O

```scheme
(open-input-file "file.txt")
(open-output-file "out.txt")
(close-input-file port)
(close-output-file port)
(read port)
(write obj port)
(read-contents "file.txt")
(read-all "file.scm")
```

#### 高阶函数

```scheme
(apply + '(1 2 3))  ; 6
```

### 标准库

解释器会自动加载`stdlib.scm`，提供以下函数：

#### 逻辑函数

```scheme
(not #t)         ; #f
(null? '())      ; #t
```

#### 列表构造

```scheme
(list 1 2 3)     ; (1 2 3)
```

#### 高阶函数

```scheme
(map (lambda (x) (* x 2)) '(1 2 3))  ; (2 4 6)
(filter odd? '(1 2 3 4))             ; (1 3)
(foldr + 0 '(1 2 3))                 ; 6
(foldl + 0 '(1 2 3))                 ; 6
```

#### 数值判断

```scheme
(zero? 0)        ; #t
(positive? 5)    ; #t
(negative? -3)   ; #t
(odd? 3)         ; #t
(even? 4)        ; #t
```

#### 列表操作

```scheme
(length '(1 2 3))        ; 3
(reverse '(1 2 3))       ; (3 2 1)
(sum 1 2 3)              ; 6
(product 2 3 4)          ; 24
(max 1 5 3)              ; 5
(min 1 5 3)              ; 1
```

#### 函数组合

```scheme
(curry + 1)              ; 返回 (lambda (x) (+ 1 x))
(flip cons)              ; 返回 (lambda (a b) (cons b a))
(compose f g)            ; 返回 (lambda (x) (f (g x)))
```

#### 查找函数

```scheme
(memq 'a '(a b c))       ; a
(memv 2 '(1 2 3))        ; 2
(member '(1) '((1) (2))) ; (1)
(assq 'a '((a 1) (b 2))) ; (a 1)
```

## 示例

### 定义函数

```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)  ; 120
```

### 使用lambda

```scheme
(define square (lambda (x) (* x x)))
(square 4)  ; 16
```

### 可变参数

```scheme
(define (sum-all . nums)
  (fold + 0 nums))

(sum-all 1 2 3 4 5)  ; 15
```

### 条件表达式

```scheme
(cond
  ((< x 0) "negative")
  ((= x 0) "zero")
  (else "positive"))
```

### 模式匹配

```scheme
(case x
  ((1 2 3) "small")
  ((4 5 6) "medium")
  (else "large"))
```

### 文件操作

```scheme
(define (load-and-eval filename)
  (load filename))

(load "stdlib.scm")
```

## 语法特性

### 转义字符

字符串支持转义：`\n`, `\r`, `\t`, `\\`, `\"`

### 引用语法

```scheme
'(1 2 3)         ; quote
`(1 ,x ,@lst)    ; quasiquote, unquote, unquote-splicing
```

### 注释

使用分号开始单行注释：

```scheme
; 这是注释
(define x 10)  ; 行尾注释
```

## 错误处理

解释器会报告以下类型的错误：

- 参数个数错误
- 类型不匹配
- 解析错误
- 未定义的变量
- 错误的特殊形式

## 许可证

MIT License
