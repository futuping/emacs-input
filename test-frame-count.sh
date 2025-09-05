#!/bin/bash

echo "🧪 Testing frame count for emacs-input-fast"
echo "==========================================="

# Function to count frames
count_frames() {
    emacsclient -e "(length (frame-list))" 2>/dev/null | tr -d ' \n'
}

echo "1. Initial frame count:"
INITIAL_COUNT=$(count_frames)
echo "   Frames: $INITIAL_COUNT"

echo ""
echo "2. Testing emacs-input-fast with -c flag:"
echo "   Command: emacsclient -c -e \"(emacs-input-fast)\""

# Test the fast version
emacsclient -c -e "(progn (load-file \"emacs-input.el\") (emacs-input-fast))" > /dev/null 2>&1 &
FAST_PID=$!

# Wait a moment for frame to be created
sleep 2

FAST_COUNT=$(count_frames)
echo "   Frames after emacs-input-fast: $FAST_COUNT"

# Calculate difference
FAST_DIFF=$((FAST_COUNT - INITIAL_COUNT))
echo "   New frames created: $FAST_DIFF"

if [ "$FAST_DIFF" -eq 1 ]; then
    echo "   ✅ Perfect! Only 1 frame created (as expected)"
elif [ "$FAST_DIFF" -eq 2 ]; then
    echo "   ❌ Problem: 2 frames created (client + emacs-input frame)"
else
    echo "   ⚠️  Unexpected: $FAST_DIFF frames created"
fi

echo ""
echo "3. Comparison with original emacs-input:"
echo "   Command: emacsclient -e \"(emacs-input)\""

# Test original version for comparison
emacsclient -e "(emacs-input)" > /dev/null 2>&1 &
ORIG_PID=$!

sleep 2

ORIG_COUNT=$(count_frames)
echo "   Frames after original emacs-input: $ORIG_COUNT"

ORIG_DIFF=$((ORIG_COUNT - FAST_COUNT))
echo "   Additional frames from original: $ORIG_DIFF"

echo ""
echo "📊 Summary:"
echo "   - Initial frames: $INITIAL_COUNT"
echo "   - After emacs-input-fast: $FAST_COUNT (diff: +$FAST_DIFF)"
echo "   - After original emacs-input: $ORIG_COUNT (diff: +$ORIG_DIFF from fast)"

if [ "$FAST_DIFF" -eq 1 ]; then
    echo "   🎉 SUCCESS: emacs-input-fast creates exactly 1 frame!"
else
    echo "   🔧 NEEDS FIX: emacs-input-fast creates $FAST_DIFF frames"
fi
