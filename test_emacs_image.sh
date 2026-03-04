#!/bin/bash

IMAGE_NAME="lain-emacs:latest"
EMACS_PATH="/nix/store/emacs/bin/emacs"

echo "--- Running Emacs regression suite for $IMAGE_NAME ---"

# Function to run elisp code using a temporary file
run_elisp_code() {
    ELISP_CODE="$1"
    TEST_NAME="$2"
    TEMP_FILE="temp_elisp_test_${TEST_NAME}.el"
    EMACS_CONFIG_FILE="emacs" # This is the file that gets copied to /root/.emacs

    echo "$ELISP_CODE" > "$TEMP_FILE"
    chmod +r "$TEMP_FILE" # Ensure file is readable by docker

    OUTPUT=$(docker run --rm \
        -v "$(pwd)/$TEMP_FILE":/app/"$TEMP_FILE" \
        -v "$(pwd)/$EMACS_CONFIG_FILE":/root/.emacs \
        $IMAGE_NAME $EMACS_PATH --load /root/.emacs --script /app/"$TEMP_FILE")
    EXIT_CODE=$?

    rm "$TEMP_FILE" # Clean up temporary file

    if [ $EXIT_CODE -ne 0 ]; then
        echo "   $TEST_NAME FAILED (Exit Code: $EXIT_CODE, Output: $OUTPUT)"
        return 1
    fi
    echo "$OUTPUT"
    return 0
}

# Define a function to test an individual Emacs Lisp extension
test_extension() {
    EXTENSION_NAME="$1"
    MESSAGE_TO_CHECK="${EXTENSION_NAME} loaded"
    ELISP_CODE="(progn (require '${EXTENSION_NAME}) (print \"${MESSAGE_TO_CHECK}\"))"

    echo "Checking ${EXTENSION_NAME} extension..."
    OUTPUT=$(run_elisp_code "$ELISP_CODE" "$EXTENSION_NAME")
    if [ $? -ne 0 ]; then
        exit 1
    fi

    if echo "$OUTPUT" | grep -q "${MESSAGE_TO_CHECK}"; then
        echo "   ${EXTENSION_NAME} extension check PASSED"
    else
        echo "   ${EXTENSION_NAME} extension check FAILED (Output: $OUTPUT)"
        exit 1
    fi
}

# 1. Emacs version check
echo "1. Checking Emacs version..."
VERSION_OUTPUT=$(run_elisp_code "(print (emacs-version))" "version")
if [ $? -ne 0 ]; then
    exit 1
fi

if echo "$VERSION_OUTPUT" | grep -q "31.0.50"; then
    echo "   Emacs version check PASSED (Expected 31.0.50, Got: $VERSION_OUTPUT)"
else
    echo "   Emacs version check FAILED (Expected 31.0.50, Got: $VERSION_OUTPUT)"
    exit 1
fi

# 2. Existing extension checks (Dash, Htmlize, Lain)
echo "2. Checking Dash extension..."
DASH_OUTPUT=$(run_elisp_code "(progn (require 'dash) (print (-map (lambda (x) (* x 2)) '(1 2 3))))" "dash")
if [ $? -ne 0 ]; then
    exit 1
fi

if echo "$DASH_OUTPUT" | grep -q "(2 4 6)"; then
    echo "   Dash extension check PASSED"
else
    echo "   Dash extension check FAILED (Output: $DASH_OUTPUT)"
    exit 1
fi

echo "3. Checking Htmlize extension..."
HTMLIZE_OUTPUT=$(run_elisp_code "(progn (require 'htmlize) (print \"htmlize loaded\"))" "htmlize")
if [ $? -ne 0 ]; then
    exit 1
fi

if echo "$HTMLIZE_OUTPUT" | grep -q "htmlize loaded"; then
    echo "   Htmlize extension check PASSED"
else
    echo "   Htmlize extension check FAILED (Output: $HTMLIZE_OUTPUT)"
    exit 1
fi

echo "4. Checking Lain extension..."
LAIN_OUTPUT=$(run_elisp_code "(progn (require 'lain) (print \"lain loaded\") (fboundp 'lain-create-agenda-view))" "lain")
if [ $? -ne 0 ]; then
    exit 1
fi

if echo "$LAIN_OUTPUT" | grep -q "lain loaded" && echo "$LAIN_OUTPUT" | grep -q "t"; then
    echo "   Lain extension check PASSED"
else
    echo "   Lain extension check FAILED (Output: $LAIN_OUTPUT)"
    exit 1
fi

# 5. New extension checks
test_extension "web"
test_extension "s"
test_extension "noflet"
test_extension "kv"
test_extension "fakir"
test_extension "elnode"
test_extension "db"
test_extension "creole"

echo "--- All regression checks PASSED ---"