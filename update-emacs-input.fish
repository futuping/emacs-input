#!/usr/bin/env fish

# ====================================================================
# Update emacs-input and restart Emacs service (Fish version)
# 更新 emacs-input 并重启 Emacs 服务 (Fish 版本)
# ====================================================================

# Colors for output / 输出颜色
set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set BLUE '\033[0;34m'
set NC '\033[0m' # No Color / 无颜色

# Function to print colored messages / 彩色输出函数
function print_info
    echo -e "$BLUE""[INFO]""$NC $argv"
end

function print_success
    echo -e "$GREEN""[SUCCESS]""$NC $argv"
end

function print_warning
    echo -e "$YELLOW""[WARNING]""$NC $argv"
end

function print_error
    echo -e "$RED""[ERROR]""$NC $argv"
end

# ====================================================================
# Step 1: Update emacs-input repository
# 步骤 1: 更新 emacs-input 仓库
# ====================================================================

print_info "Updating emacs-input repository..."
print_info "更新 emacs-input 仓库..."

# Check if we're in the emacs-input directory
if not test -f "emacs-input.el"
    print_error "Not in emacs-input directory. Please run from the project root."
    print_error "不在 emacs-input 目录中。请从项目根目录运行。"
    exit 1
end

# Fetch latest changes / 获取最新更改
print_info "Fetching from origin..."
git fetch origin

# Check current status / 检查当前状态
set CURRENT_BRANCH (git branch --show-current)
print_info "Current branch: $CURRENT_BRANCH"

# Check if there are diverged commits / 检查是否有分歧的提交
set LOCAL_COMMIT (git rev-parse HEAD)
set REMOTE_COMMIT (git rev-parse origin/$CURRENT_BRANCH)

if test "$LOCAL_COMMIT" != "$REMOTE_COMMIT"
    print_warning "Local and remote have diverged. Pulling latest changes..."
    print_warning "本地和远程有分歧。拉取最新更改..."
    
    # Pull latest changes / 拉取最新更改
    git pull origin $CURRENT_BRANCH
    print_success "Repository updated to latest version"
    print_success "仓库已更新到最新版本"
else
    print_info "Repository is already up to date"
    print_info "仓库已是最新"
end

# Show current commit / 显示当前提交
echo ""
print_info "Current commit:"
git log --oneline -1

# ====================================================================
# Step 2: Update Hammerspoon configuration
# 步骤 2: 更新 Hammerspoon 配置
# ====================================================================

echo ""
print_info "Updating Hammerspoon configuration..."
print_info "更新 Hammerspoon 配置..."

set HAMMERSPOON_DIR "$HOME/.hammerspoon"

if test -d "$HAMMERSPOON_DIR"
    # Copy updated Hammerspoon script / 复制更新的 Hammerspoon 脚本
    cp "hammerspoon/emacs-input.lua" "$HAMMERSPOON_DIR/"
    print_success "Hammerspoon script updated"
    print_success "Hammerspoon 脚本已更新"
    
    # Reload Hammerspoon configuration / 重新加载 Hammerspoon 配置
    if command -v hs > /dev/null
        hs -c "hs.reload()"
        print_success "Hammerspoon configuration reloaded"
        print_success "Hammerspoon 配置已重新加载"
    else
        print_warning "Hammerspoon CLI not found. Please reload manually."
        print_warning "未找到 Hammerspoon CLI。请手动重新加载。"
    end
else
    print_warning "Hammerspoon directory not found. Run setup-hammerspoon.sh first."
    print_warning "未找到 Hammerspoon 目录。请先运行 setup-hammerspoon.sh。"
end

# ====================================================================
# Step 3: Restart Emacs service (nix-darwin)
# 步骤 3: 重启 Emacs 服务 (nix-darwin)
# ====================================================================

echo ""
print_info "Restarting Emacs service..."
print_info "重启 Emacs 服务..."

# Check if service exists / 检查服务是否存在
set SERVICE_NAME "org.nixos.emacs"
set OLD_PID (launchctl list | grep "$SERVICE_NAME" | awk '{print $1}')

if test -z "$OLD_PID"; or test "$OLD_PID" = "-"
    print_warning "Emacs service not running or not found"
    print_warning "Emacs 服务未运行或未找到"
else
    print_info "Current Emacs daemon PID: $OLD_PID"
    
    # Stop service / 停止服务
    launchctl stop "$SERVICE_NAME"
    sleep 1
    
    # Start service / 启动服务
    launchctl start "$SERVICE_NAME"
    sleep 2
    
    # Verify new PID / 验证新 PID
    set NEW_PID (launchctl list | grep "$SERVICE_NAME" | awk '{print $1}')
    
    if test -n "$NEW_PID"; and test "$NEW_PID" != "-"; and test "$NEW_PID" != "$OLD_PID"
        print_success "Emacs service restarted (PID: $OLD_PID → $NEW_PID)"
        print_success "Emacs 服务已重启 (PID: $OLD_PID → $NEW_PID)"
    else
        print_error "Failed to restart Emacs service"
        print_error "重启 Emacs 服务失败"
        exit 1
    end
end

# ====================================================================
# Step 4: Verify the update
# 步骤 4: 验证更新
# ====================================================================

echo ""
print_info "Verifying update..."
print_info "验证更新..."

# Check if emacsclient can connect / 检查 emacsclient 是否能连接
if not emacsclient --eval '(emacs-version)' > /dev/null 2>&1
    print_error "Cannot connect to Emacs daemon"
    print_error "无法连接到 Emacs daemon"
    exit 1
end

# Check emacs-input status / 检查 emacs-input 状态
set LOADED (emacsclient --eval '(if (featurep \'emacs-input) "yes" "no")' 2>/dev/null | tr -d '"')
if test "$LOADED" = "yes"
    print_success "emacs-input is loaded"
    print_success "emacs-input 已加载"
else
    print_warning "emacs-input is not loaded (may need manual loading)"
    print_warning "emacs-input 未加载（可能需要手动加载）"
end

# Check if frame is initialized / 检查 frame 是否已初始化
set FRAME_READY (emacsclient --eval '(if (and (boundp \'emacs-input--frame) emacs-input--frame) "yes" "no")' 2>/dev/null | tr -d '"')
if test "$FRAME_READY" = "yes"
    print_success "emacs-input frame is ready"
    print_success "emacs-input frame 已就绪"
else
    print_info "Initializing emacs-input frame..."
    print_info "初始化 emacs-input frame..."
    emacsclient --eval '(emacs-input-initialize)' > /dev/null 2>&1
end

# ====================================================================
# Summary / 总结
# ====================================================================

echo ""
echo "======================================================================"
print_success "Update completed! / 更新完成！"
echo "======================================================================"
echo ""
print_info "Project location / 项目位置: "(pwd)
print_info "Hammerspoon config / Hammerspoon 配置: ~/.hammerspoon/emacs-input.lua"
echo ""
print_info "Test the update by pressing: ⌘⇧E (Cmd+Shift+E)"
print_info "通过按键测试更新: ⌘⇧E (Cmd+Shift+E)"
echo ""
print_info "If emacs-input is not working, check:"
print_info "如果 emacs-input 无法工作，请检查："
echo "  1. Emacs server is running: M-x server-start"
echo "  2. emacs-input is loaded: (require 'emacs-input)"
echo "  3. Hammerspoon has accessibility permissions"
echo "  4. Hammerspoon configuration is reloaded"
echo ""
