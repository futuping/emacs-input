#!/bin/bash

# Hammerspoon 配置脚本

echo "配置 Hammerspoon for emacs-input..."

# 检查 Hammerspoon 是否安装
HS_CMD=""
if command -v hs &> /dev/null; then
    HS_CMD="hs"
elif [ -f "/nix/store/2l8mcmysihrdbs85hv53ymhl1mh03kqs-hammerspoon-1.0.0/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs" ]; then
    HS_CMD="/nix/store/2l8mcmysihrdbs85hv53ymhl1mh03kqs-hammerspoon-1.0.0/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs"
elif [ -d "/Applications/Hammerspoon.app" ]; then
    HS_CMD="/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs"
else
    echo "❌ Hammerspoon 未找到"
    echo "请确保 Hammerspoon 已正确安装"
    exit 1
fi

echo "✅ 找到 Hammerspoon: $HS_CMD"

# 创建 Hammerspoon 配置目录
HAMMERSPOON_DIR="$HOME/.hammerspoon"
mkdir -p "$HAMMERSPOON_DIR"

# 复制 emacs-input.lua
echo "📁 复制 emacs-input.lua..."
cp "hammerspoon/emacs-input.lua" "$HAMMERSPOON_DIR/"

# 处理 init.lua
INIT_LUA="$HAMMERSPOON_DIR/init.lua"

if [ ! -f "$INIT_LUA" ]; then
    echo "📝 创建新的 init.lua..."
    cat > "$INIT_LUA" << 'EOF'
-- Hammerspoon 配置文件
-- 加载 emacs-input
local emacs_input = require('emacs-input')
emacs_input.setup()

hs.alert.show("Hammerspoon 配置已加载")
EOF
else
    # 检查是否已经配置了 emacs-input
    if ! grep -q "emacs-input" "$INIT_LUA"; then
        echo "📝 添加 emacs-input 到现有 init.lua..."
        cat >> "$INIT_LUA" << 'EOF'

-- emacs-input 配置
local emacs_input = require('emacs-input')
emacs_input.setup()
EOF
    else
        echo "✅ emacs-input 已在 init.lua 中配置"
    fi
fi

echo ""
echo "✅ Hammerspoon 配置完成！"
echo ""
echo "下一步："
echo "1. 打开 Hammerspoon 应用"
echo "2. 点击菜单栏中的 Hammerspoon 图标"
echo "3. 选择 'Reload Config' 重新加载配置"
echo "4. 确保 Hammerspoon 有辅助功能权限："
echo "   系统偏好设置 > 安全性与隐私 > 隐私 > 辅助功能"
echo "5. 测试快捷键：⌘⇧E (Cmd+Shift+E)"
echo ""
echo "配置文件位置："
echo "- ~/.hammerspoon/init.lua"
echo "- ~/.hammerspoon/emacs-input.lua"
