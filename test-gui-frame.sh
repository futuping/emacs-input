#!/bin/bash

echo "🧪 Testing GUI frame creation for emacs-input-fast"
echo "================================================="

# Load the updated emacs-input.el
echo "1. Loading emacs-input.el..."
emacsclient -e "(load-file \"emacs-input.el\")" > /dev/null 2>&1
echo "   ✅ Loaded"

# Test emacs-input-fast
echo ""
echo "2. Testing emacs-input-fast..."
echo "   Command: emacsclient -e \"(emacs-input-fast)\""

# Count frames before
BEFORE=$(emacsclient -e "(length (frame-list))" 2>/dev/null | tr -d ' \n')
echo "   Frames before: $BEFORE"

# Execute emacs-input-fast
RESULT=$(emacsclient -e "(emacs-input-fast)" 2>&1)
echo "   Result: $RESULT"

# Wait a moment for frame creation
sleep 2

# Count frames after
AFTER=$(emacsclient -e "(length (frame-list))" 2>/dev/null | tr -d ' \n')
echo "   Frames after: $AFTER"

# Calculate difference
DIFF=$((AFTER - BEFORE))
echo "   New frames: $DIFF"

if [ "$DIFF" -eq 1 ]; then
    echo "   ✅ SUCCESS: GUI frame created!"
elif [ "$DIFF" -eq 0 ]; then
    echo "   ❌ PROBLEM: No GUI frame created"
else
    echo "   ⚠️  UNEXPECTED: $DIFF frames created"
fi

echo ""
echo "3. Comparison with original emacs-input..."
echo "   Command: emacsclient -e \"(emacs-input)\""

# Test original for comparison
ORIG_BEFORE=$AFTER
ORIG_RESULT=$(emacsclient -e "(emacs-input)" 2>&1)
echo "   Result: $ORIG_RESULT"

sleep 2

ORIG_AFTER=$(emacsclient -e "(length (frame-list))" 2>/dev/null | tr -d ' \n')
ORIG_DIFF=$((ORIG_AFTER - ORIG_BEFORE))

echo "   Original frames created: $ORIG_DIFF"

echo ""
echo "📊 Summary:"
echo "   - emacs-input-fast: $DIFF frame(s) created"
echo "   - original emacs-input: $ORIG_DIFF frame(s) created"

if [ "$DIFF" -eq 1 ] && [ "$ORIG_DIFF" -eq 1 ]; then
    echo "   🎉 PERFECT: Both create exactly 1 GUI frame!"
elif [ "$DIFF" -eq 0 ]; then
    echo "   🔧 FIX NEEDED: emacs-input-fast not creating GUI frame"
else
    echo "   ⚠️  REVIEW NEEDED: Unexpected behavior"
fi
