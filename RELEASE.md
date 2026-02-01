# Release Guide

本文档说明如何使用GitHub Actions自动发布多平台版本。

## 支持的平台

自动构建系统支持以下平台：

1. **Windows x86_64** - `.zip`格式
2. **macOS ARM64** (Apple Silicon) - `.tar.gz`格式
3. **Linux x86_64** - `.tar.gz`, `.deb`, `.rpm`格式
4. **Linux ARM64** - `.tar.gz`, `.deb`, `.rpm`格式

## 发布流程

### 1. 创建版本标签

```bash
# 确保所有更改已提交
git add .
git commit -m "准备发布 v0.1.0"

# 创建版本标签
git tag v0.1.0

# 推送标签到GitHub
git push origin v0.1.0
```

### 2. 自动构建

推送标签后，GitHub Actions会自动：

1. 在Windows、macOS和Linux平台上构建可执行文件
2. 使用QEMU构建Linux ARM64版本
3. 创建Linux的deb和rpm包
4. 生成所有文件的SHA256校验和
5. 创建GitHub Release并上传所有文件

### 3. 查看发布

访问 `https://github.com/Africar-Lee/ownScheme/releases` 查看发布的版本。

## 发布文件说明

每个版本会生成以下文件：

### Windows
- `carScheme-v0.1.0-windows-x86_64.zip` - Windows可执行文件包
- `carScheme-v0.1.0-windows-x86_64.zip.sha256` - 校验和

### macOS
- `carScheme-v0.1.0-macos-arm64.tar.gz` - macOS可执行文件包
- `carScheme-v0.1.0-macos-arm64.tar.gz.sha256` - 校验和

### Linux x86_64
- `carScheme-v0.1.0-linux-x86_64.tar.gz` - 通用Linux包
- `carscheme_0.1.0_amd64.deb` - Debian/Ubuntu包
- `carscheme-0.1.0-1.x86_64.rpm` - RedHat/Fedora/CentOS包
- 对应的`.sha256`校验和文件

### Linux ARM64
- `carScheme-v0.1.0-linux-arm64.tar.gz` - 通用Linux包
- `carscheme_0.1.0_arm64.deb` - Debian/Ubuntu包
- `carscheme-0.1.0-1.arm64.rpm` - RedHat/Fedora/CentOS包
- 对应的`.sha256`校验和文件

## 安装说明

### Windows
1. 下载`.zip`文件
2. 解压到任意目录
3. 将目录添加到PATH环境变量
4. 运行`carScheme.exe`

### macOS
```bash
# 下载并解压
tar xzf carScheme-v0.1.0-macos-arm64.tar.gz
cd carScheme-v0.1.0-macos-arm64

# 移动到系统路径
sudo cp carScheme /usr/local/bin/
sudo mkdir -p /usr/local/share/carscheme
sudo cp stdlib.scm /usr/local/share/carscheme/

# 运行
carScheme
```

### Linux (通用包)
```bash
# 下载并解压
tar xzf carScheme-v0.1.0-linux-x86_64.tar.gz
cd carScheme-v0.1.0-linux-x86_64

# 移动到系统路径
sudo cp carScheme /usr/local/bin/
sudo mkdir -p /usr/local/share/carscheme
sudo cp stdlib.scm /usr/local/share/carscheme/

# 运行
carScheme
```

### Linux (DEB包)
```bash
# Debian/Ubuntu
sudo dpkg -i carscheme_0.1.0_amd64.deb

# 或使用apt
sudo apt install ./carscheme_0.1.0_amd64.deb

# 运行
carScheme
```

### Linux (RPM包)
```bash
# RedHat/Fedora/CentOS
sudo rpm -i carscheme-0.1.0-1.x86_64.rpm

# 或使用dnf
sudo dnf install carscheme-0.1.0-1.x86_64.rpm

# 运行
carScheme
```

## 验证下载文件

使用SHA256校验和验证文件完整性：

```bash
# Linux/macOS
sha256sum -c carScheme-v0.1.0-linux-x86_64.tar.gz.sha256

# Windows (PowerShell)
$hash = Get-FileHash carScheme-v0.1.0-windows-x86_64.zip -Algorithm SHA256
$expected = Get-Content carScheme-v0.1.0-windows-x86_64.zip.sha256
if ($hash.Hash -eq $expected.Split()[0]) { "OK" } else { "FAILED" }
```

## 版本号规范

遵循语义化版本规范 (Semantic Versioning)：

- `v1.0.0` - 主版本号.次版本号.修订号
- `v1.0.0-beta.1` - 预发布版本
- `v1.0.0-rc.1` - 候选发布版本

## 故障排查

### 构建失败

1. 检查GitHub Actions日志
2. 确保所有依赖在cabal文件中正确声明
3. 验证GHC和Cabal版本兼容性

### Linux ARM64构建慢

ARM64构建使用QEMU模拟，速度较慢是正常现象。通常需要15-30分钟。

### 包安装失败

确保系统满足依赖要求。Linux静态链接版本应该可以在大多数系统上运行。

## 手动构建

如果需要手动构建特定平台：

```bash
# 克隆仓库
git clone https://github.com/Africar-Lee/ownScheme.git
cd ownScheme

# 构建
cabal build

# 安装到本地
cabal install --installdir=./bin --install-method=copy
```

## 配置说明

### 修改GHC版本

编辑`.github/workflows/release.yml`中的`ghc`字段：

```yaml
ghc: '9.6.7'  # 修改为所需版本
```

### 添加新平台

在workflow的matrix中添加新配置：

```yaml
- os: linux
  arch: aarch64
  runner: ubuntu-latest
  ghc: '9.6.7'
  cabal: '3.10'
```

### 修改包信息

编辑workflow中的fpm命令参数来修改包的元数据。

## 注意事项

1. **stdlib.scm依赖**：所有发布包都包含stdlib.scm文件
2. **静态链接**：Linux版本使用静态链接，减少依赖问题
3. **权限**：确保GitHub仓库有写入权限（Settings > Actions > General > Workflow permissions）
4. **标签格式**：必须以`v`开头（如`v0.1.0`）才会触发发布流程
