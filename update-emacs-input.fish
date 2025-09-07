#!/usr/bin/env fish

# ====================================================================
# 更新 emacs-input 并重启 Emacs 服务 (Fish 版本)
# ====================================================================

# 输出颜色
set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set BLUE '\033[0;34m'
set NC '\033[0m' # 无颜色

# 彩色输出函数
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
# 步骤 1: 通过 straight.el 更新 emacs-input 仓库
# ====================================================================

print_info "通过 straight.el 更新 emacs-input..."

# 检查 emacsclient 是否能连接
if not emacsclient --eval '(emacs-version)' > /dev/null 2>&1
    print_error "无法连接到 Emacs daemon。请先启动 Emacs 服务。"
    print_info "启动 Emacs 服务：M-x server-start 或 (server-start)"
    exit 1
end

# 检查 straight.el 是否可用
set STRAIGHT_AVAILABLE (emacsclient --eval '(if (fboundp '\''straight-pull-package) "yes" "no")' 2>/dev/null | tr -d '"')
if test "$STRAIGHT_AVAILABLE" != "yes"
    print_error "straight.el 不可用或未加载"
    print_info "请确保在您的 Emacs 中正确配置了 straight.el"
    exit 1
end

print_success "straight.el 可用"

# 通过 straight.el 更新 emacs-input 包
print_info "拉取 emacs-input 包的最新更改..."

set UPDATE_RESULT (emacsclient --eval '(condition-case err (progn (straight-pull-package "emacs-input") "success") (error (format "error: %s" err)))' 2>/dev/null | tr -d '"')

if test "$UPDATE_RESULT" = "success"
    print_success "emacs-input 包更新成功"
else if string match -q "error:*" "$UPDATE_RESULT"
    print_error "更新 emacs-input 包失败：$UPDATE_RESULT"
    exit 1
else
    print_warning "更新完成，结果：$UPDATE_RESULT"
end

# 如果需要则重新构建包
print_info "重新构建 emacs-input 包..."

set REBUILD_RESULT (emacsclient --eval '(condition-case err (progn (straight-rebuild-package "emacs-input") "success") (error (format "error: %s" err)))' 2>/dev/null | tr -d '"')

if test "$REBUILD_RESULT" = "success"
    print_success "emacs-input 包重新构建成功"
else if string match -q "error:*" "$REBUILD_RESULT"
    print_warning "重新构建 emacs-input 包失败：$REBUILD_RESULT"
    print_info "如果包工作正常，这可能不是关键问题"
else
    print_info "重新构建完成，结果：$REBUILD_RESULT"
end

# 显示包信息
echo ""
print_info "包信息："
set PACKAGE_INFO (emacsclient --eval '(expand-file-name "emacs-input" (straight--repos-dir))' 2>/dev/null | tr -d '"')
if test -n "$PACKAGE_INFO"
    print_info "包位置：$PACKAGE_INFO"
end

# ====================================================================
# 步骤 2: 更新 Hammerspoon 配置
# ====================================================================

echo ""
print_info "更新 Hammerspoon 配置..."

set HAMMERSPOON_DIR "$HOME/.hammerspoon"

if test -d "$HAMMERSPOON_DIR"
    # 复制更新的 Hammerspoon 脚本
    cp "hammerspoon/emacs-input.lua" "$HAMMERSPOON_DIR/"
    print_success "Hammerspoon 脚本已更新"

    # 重新加载 Hammerspoon 配置
    set HS_CMD ""

    # 在各种位置查找 hs 命令
    if command -v hs > /dev/null
        set HS_CMD "hs"
    else if test -f "/Applications/Nix Apps/Hammerspoon.app/Contents/Frameworks/hs/hs"
        set HS_CMD "/Applications/Nix Apps/Hammerspoon.app/Contents/Frameworks/hs/hs"
    else if test -f "/nix/store/2l8mcmysihrdbs85hv53ymhl1mh03kqs-hammerspoon-1.0.0/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs"
        set HS_CMD "/nix/store/2l8mcmysihrdbs85hv53ymhl1mh03kqs-hammerspoon-1.0.0/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs"
    else if test -f "/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs"
        set HS_CMD "/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs"
    end

    if test -n "$HS_CMD"
        # 尝试重新加载 Hammerspoon 配置
        if "$HS_CMD" -c "hs.reload()" 2>/dev/null
            print_success "Hammerspoon 配置已重新加载"
        else
            print_warning "找到 Hammerspoon CLI 但重新加载失败。请手动重新加载。"
            print_info "手动重新加载：点击 Hammerspoon 菜单 → Reload Config"
        end
    else
        print_warning "未找到 Hammerspoon CLI。请手动重新加载。"
    end
else
    print_warning "未找到 Hammerspoon 目录。请先运行 setup-hammerspoon.sh。"
end

# ====================================================================
# 步骤 3: 重启 Emacs 服务 (nix-darwin)
# ====================================================================

echo ""
print_info "重启 Emacs 服务..."

# 检查服务是否存在
set SERVICE_NAME "org.nixos.emacs"
set PLIST_FILE "org.nixos.emacs.plist"
set LAUNCH_AGENTS_DIR "$HOME/Library/LaunchAgents"
set OLD_PID (launchctl list | grep "$SERVICE_NAME" | awk '{print $1}')

if test -z "$OLD_PID"; or test "$OLD_PID" = "-"
    print_warning "Emacs 服务未运行或未找到"
else
    print_info "当前 Emacs daemon PID: $OLD_PID"

    # 卸载服务
    print_info "卸载 Emacs 服务..."
    launchctl unload "$LAUNCH_AGENTS_DIR/$PLIST_FILE"
    sleep 2

    # 加载服务
    print_info "加载 Emacs 服务..."
    launchctl load "$LAUNCH_AGENTS_DIR/$PLIST_FILE"
    sleep 3

    # 验证新 PID
    set NEW_PID (launchctl list | grep "$SERVICE_NAME" | awk '{print $1}')

    if test -n "$NEW_PID"; and test "$NEW_PID" != "-"; and test "$NEW_PID" != "$OLD_PID"
        print_success "Emacs 服务已重启 (PID: $OLD_PID → $NEW_PID)"

        # 等待daemon完全启动然后启动server
        print_info "启动 Emacs server..."
        sleep 3
        emacsclient --eval '(server-start)' > /dev/null 2>&1
        print_success "Emacs server 已启动"
    else
        print_error "重启 Emacs 服务失败"
        exit 1
    end
end

# ====================================================================
# 步骤 4: 验证更新
# ====================================================================

echo ""
print_info "验证更新..."

# 检查 emacsclient 是否能连接
if not emacsclient --eval '(emacs-version)' > /dev/null 2>&1
    print_error "无法连接到 Emacs daemon"
    exit 1
end

# 检查 emacs-input 状态
set LOADED (emacsclient --eval '(if (featurep \'emacs-input) "yes" "no")' 2>/dev/null | tr -d '"')
if test "$LOADED" = "yes"
    print_success "emacs-input 已加载"
else
    print_warning "emacs-input 未加载（可能需要手动加载）"
end

# 检查 frame 是否已初始化
set FRAME_READY (emacsclient --eval '(if (and (boundp \'emacs-input--frame) emacs-input--frame (frame-live-p emacs-input--frame)) "yes" "no")' 2>/dev/null | tr -d '"')
if test "$FRAME_READY" = "yes"
    print_success "emacs-input frame 已就绪"
else
    print_info "初始化 emacs-input frame 以实现快速访问..."
    emacsclient --eval '(emacs-input-initialize-frame)' > /dev/null 2>&1
    print_success "快速 frame 已初始化"
end

# ====================================================================
# 总结
# ====================================================================

echo ""
echo "======================================================================"
print_success "更新完成！"
echo "======================================================================"
echo ""
print_info "项目位置: "(pwd)
print_info "Hammerspoon 配置: ~/.hammerspoon/emacs-input.lua"
echo ""
print_info "通过按键测试更新: ⌘⇧E (Cmd+Shift+E)"
echo ""
print_info "如果 emacs-input 无法工作，请检查："
echo "  1. Emacs server 正在运行: M-x server-start"
echo "  2. emacs-input 已加载: (require 'emacs-input)"
echo "  3. Hammerspoon 有辅助功能权限"
echo "  4. Hammerspoon 配置已重新加载"
echo ""
