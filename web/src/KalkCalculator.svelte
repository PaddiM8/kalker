<svelte:options tag="kalk-calculator" />

<script lang="ts">
    import { afterUpdate } from "svelte";
    import type { Context } from "@paddim8/kalk";
    import ConsoleLine from "./ConsoleLine.svelte";
    import * as shadow from "shadow-selection-polyfill";

    // Props, HTML doesn't recognise them if they're not like this
    export let identifiercolor = "cornflowerblue";
    export let operatorcolor = "darkorange";
    export let promptcolor = "mediumseagreen";
    export let errorcolor = "tomato";
    export let linkcolor = "cornflowerblue";
    export let hinttext = "";
    export let autofocus = false;
    export let buttonpanel = false;
    export let numberrow = false;

    type Kalk = typeof import("@paddim8/kalk");

    let outputLines: [value: string, byUser: boolean][] = [];
    let buttonRowValues = [
        "+",
        "-",
        "×",
        "÷",
        "^",
        "=",
        ".",
        "(",
        "√",
        "π",
        ",",
        "%",
        "∑",
        "⌊",
        "⌈",
        "∫",
    ];
    let numberRowValues = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"];
    let outputElement: HTMLElement;
    let kalkContext: Context;
    let selectedLineOffset: number = 0;
    let calculatorElement: HTMLElement;
    let inputElement: HTMLTextAreaElement;
    let highlightedTextElement: HTMLElement;
    let ignoreNextInput = false;
    let currentBase = 10;

    enum HighlightType {
        Output,
        InputField,
        History,
    }

    function setText(
        text: string,
        isFinalBeforeSubmit = false,
        isComposing = false
    ) {
        const [highlighted, offset] = highlight(
            text,
            isFinalBeforeSubmit
                ? HighlightType.History
                : HighlightType.InputField
        );
        const prevCursorPos = inputElement.selectionStart;
        setHtml(highlighted);
        if (!isComposing) {
            setCaret(prevCursorPos + offset);
        }
    }

    function setHtml(html: string) {
        highlightedTextElement.innerHTML = html;
        inputElement.value = highlightedTextElement.innerText;

        if (!html) {
            highlightedTextElement.innerHTML = `<span class='placeholder'>${hinttext}</div>`;
        }
    }

    function getHtml(): string {
        return highlightedTextElement.innerHTML;
    }

    function setCaret(pos: number) {
        inputElement.selectionStart = pos;
        inputElement.selectionEnd = inputElement.selectionStart;
    }

    function offsetCaret(offset: number) {
        setCaret(inputElement.selectionStart + offset);
    }

    function calculate(
        kalk: Kalk,
        input: string
    ): [result: string, success: boolean] {
        try {
            if (!kalkContext) kalkContext = new kalk.Context();
            const result = kalkContext.evaluate(input.replaceAll(/\s+/g, " "));
            if (result && !result.setRadix(currentBase)) {
                return ["Invalid base", false];
            }

            return [result?.toPrettyString(), true];
        } catch (err) {
            return [err, false];
        }
    }

    function hasUnevenAmountOfBrackets(
        input: string,
        openChar: string,
        closedChar: string,
        onlyCheckFirstLine = false
    ): boolean {
        let openCount = 0;
        let closedCount = 0;
        for (const char of input) {
            if (onlyCheckFirstLine && char == "\n") break;
            if (char == openChar) openCount++;
            if (char == closedChar) closedCount++;
        }

        return openCount > closedCount;
    }

    function handleKeyDown(event: KeyboardEvent, kalk: Kalk) {
        if (event.key == "Enter") {
            if (
                hasUnevenAmountOfBrackets(
                    (event.target as HTMLTextAreaElement).value,
                    "{",
                    "}"
                ) ||
                hasUnevenAmountOfBrackets(
                    (event.target as HTMLTextAreaElement).value,
                    "[",
                    "]"
                )
            ) {
                return;
            }

            selectedLineOffset = 0;
            const input = inputElement.value;
            let output: string;

            if (input == "") {
                return;
            }

            if (input.trim() == "help") {
                output = `<a style="color: ${linkcolor}"
                             href="https://kalker.xyz/#usage"
                             target="blank">Link to usage guide</a>`;
            } else if (/base\s\d\d?/.test(input.trim())) {
                const baseInput = Number(input.trim().slice(5));
                if (baseInput <= 1 || baseInput >= 50) {
                    output = `<span style="color: ${errorcolor}">Invalid base.</span>`;
                } else {
                    currentBase = baseInput;
                }
            } else if (input.trim() == "clear") {
                outputLines = [];
                setText("");
                return;
            } else {
                let [result, success] = calculate(kalk, input);

                output = success
                    ? highlight(result, HighlightType.Output)[0]
                    : `<span style="color: ${errorcolor}">${result}</span>`;
            }

            // Highlight
            const target = event.target as HTMLInputElement;
            setText(target.value, true);

            outputLines = output
                ? [...outputLines, [getHtml(), true], [output, false]]
                : [...outputLines, [getHtml(), true]];

            setText("");

            let i = 0;
            setInterval(() => {
                if (i == 60) return;
                outputElement.children[
                    outputElement.children.length - 1
                ].scrollIntoView();

                calculatorElement.scrollIntoView(false);
                i++;
            }, 10);
        }
    }

    function handleKeyUp(event: KeyboardEvent) {
        // on keyup, since pressing the arrow up key makes the cursor go to the start
        // of the input field. This piece of code will put the cursor at the end,
        // which therefore will need to be done afterwards, so that it doesn't just get moved back again.
        if (event.key == "ArrowUp" || event.key == "ArrowDown") {
            // Don't do anything if it's multi-line
            if (inputElement.value.match(/\r|\r\n|\n/)) return;

            const change = event.key == "ArrowUp" ? 1 : -1;
            selectedLineOffset += change;

            if (selectedLineOffset < 0) {
                setText("");
                selectedLineOffset = 0;
                return;
            }

            const index = outputLines.length - selectedLineOffset - 1;
            let line = outputLines[index];

            // If it was just a response, get the one above that instead
            while (line && !line[1]) {
                line = outputLines[index - change];
                selectedLineOffset += change;
            }

            if (line) {
                setHtml(line[0]);
            }

            if (selectedLineOffset >= outputLines.length) {
                selectedLineOffset = outputLines.length - 1;
            }
        }
    }

    function handleInput(e: Event) {
        if (ignoreNextInput) {
            ignoreNextInput = false;
            return;
        }

        const event = e as InputEvent;
        const target = event.target as HTMLInputElement;
        setText(
            target.value == "\n" ? "" : target.value,
            undefined,
            event.isComposing
        );

        if (event.data == "(") {
            insertText(")");
            offsetCaret(-1);
        }
    }

    function handleButtonClick(event: Event) {
        const target = event.target as HTMLElement;
        target.blur();
        insertText(target.textContent);
    }

    function handleArrowClick(event: Event, left: boolean) {
        const length = inputElement.value.length;
        const selection = inputElement.selectionEnd + (left ? -1 : 1);
        setCaret(Math.min(Math.max(selection, 0), length));
        inputElement.focus({ preventScroll: true });
    }

    function insertText(input: string) {
        let offset = 0;
        if (input == "(") {
            input += ")";
            offset = -1;
        } else if (input == "=") {
            input = " = ";
        } else if (input == "∑") {
            input += "()";
            offset = -1;
        } else if (input == "∫") {
            input += "()";
            offset = -1;
        } else if (input == "⌊") {
            input += "⌋";
            offset = -1;
        } else if (input == "⌈") {
            input += "⌉";
            offset = -1;
        } else if (input == ",") {
            input = ", ";
        }

        ignoreNextInput = true;
        inputElement.setRangeText(
            input,
            inputElement.selectionStart,
            inputElement.selectionEnd,
            "end"
        );
        setText(inputElement.value);
        ignoreNextInput = false;
        offsetCaret(offset);
        inputElement.focus({ preventScroll: true });
    }

    function handleLoad(element: HTMLElement) {
        if (autofocus) element.focus();
    }

    function highlight(
        input: string,
        highlightType: HighlightType
    ): [string, number] {
        if (!input) return ["", 0];
        let result = input;
        let offset = 0;
        result = result.replace(
            /(?<power>\^[0-9T])|(?<brackets>\[\[)|(?<radix>0[box][a-zA-Z0-9]+)|(?<comparison>(!=|[<>]=?))|(?<html>[<>&]|(\n\s*\}?|\s+))|(?<op>([+\-/*%^!≈×÷⋅∧∨¬ᵀ]|if|otherwise|and|or|mod|true|false|not)|(?<identifier>[^!-@\s_|^⌊⌋⌈⌉≈\[\]\{\}⟦⟧≠≥≤⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎×÷⋅∧∨ᵀ]+(_\d+)?)\(?)/g,
            (
                substring,
                power,
                brackets,
                _radix,
                _2,
                comparison,
                _3,
                html,
                _4,
                op,
                identifier
            ) => {
                if (power) {
                    if (substring == "^T") {
                        substring = "ᵀ";
                    }
                }

                if (brackets) {
                    if (substring == "[[") {
                        offset -= 1;

                        return "⟦⟧";
                    }
                }

                if (comparison) {
                    if (substring == "<=") return "≤";
                    if (substring == ">=") return "≥";
                    if (substring == "!=") return "≠";
                    if (substring == "<") return "&lt;";
                    if (substring == ">") return "&gt;";
                }

                if (html) {
                    if (substring == "&") return "&amp;";
                    if (substring.startsWith("\n")) {
                        if (substring.endsWith("}")) {
                            return "<br />}";
                        } else if (substring.endsWith("]")) {
                            return "<br />]";
                        } else {
                            let spaceCount = 2;
                            const unclosedBracketInFirstLine =
                                hasUnevenAmountOfBrackets(
                                    input,
                                    "[",
                                    "]",
                                    true
                                );
                            if (unclosedBracketInFirstLine) {
                                let bracketIndex = input.indexOf("[");
                                spaceCount =
                                    bracketIndex == -1
                                        ? spaceCount
                                        : bracketIndex + 1;
                            }

                            if (!substring.match(/\n\s/)) offset += spaceCount;
                            if (highlightType == HighlightType.Output) {
                                return "<br />";
                            } else if (
                                highlightType == HighlightType.InputField
                            ) {
                                return "<br />" + "&nbsp;".repeat(spaceCount);
                            } else if (highlightType == HighlightType.History) {
                                // Account for ">> "
                                return (
                                    "<br />" + "&nbsp;".repeat(spaceCount + 3)
                                );
                            }
                        }
                    }
                    if (substring.match(/\s+/)) {
                        return "&nbsp;".repeat(substring.length);
                    }
                }

                if (op && highlightType != HighlightType.Output) {
                    if (substring == "*") substring = "⋅";
                    if (substring == "/") substring = "÷";
                    if (substring == "and") substring = "∧";
                    if (substring == "or") substring = "∨";
                    if (substring == "not") substring = "¬";
                }

                if (identifier) {
                    let substringWithoutParen = substring.endsWith("(")
                        ? substring.slice(0, -1)
                        : substring;
                    let newSubstring: string = substringWithoutParen;
                    let addParen = false;
                    switch (substringWithoutParen) {
                        case "sqrt": {
                            newSubstring = "√";
                            break;
                        }
                        case "sum": {
                            newSubstring = "∑";
                            addParen = true;
                            break;
                        }
                        case "prod": {
                            newSubstring = "∏";
                            addParen = true;
                            break;
                        }
                        case "integral": {
                            newSubstring = "∫";
                            addParen = true;
                            break;
                        }
                        case "integrate": {
                            newSubstring = "∫";
                            addParen = true;
                            break;
                        }
                        case "pi": {
                            newSubstring = "π";
                            break;
                        }
                        case "phi": {
                            newSubstring = "ϕ";
                            break;
                        }
                        case "gamma": {
                            newSubstring = "Γ";
                            break;
                        }
                        case "floor": {
                            newSubstring = "⌊⌋";
                            break;
                        }
                        case "ceil": {
                            newSubstring = "⌈⌉";
                            break;
                        }
                        case "asin": {
                            newSubstring = "sin⁻¹";
                            addParen = true;
                            break;
                        }
                        case "acos": {
                            newSubstring = "cos⁻¹";
                            addParen = true;
                            break;
                        }
                        case "atan": {
                            newSubstring = "tan⁻¹";
                            addParen = true;
                            break;
                        }
                        case "acot": {
                            newSubstring = "cot⁻¹";
                            addParen = true;
                            break;
                        }
                        case "acosec": {
                            newSubstring = "cosec⁻¹";
                            addParen = true;
                            break;
                        }
                        case "asec": {
                            newSubstring = "sec⁻¹";
                            addParen = true;
                            break;
                        }
                        case "asinh": {
                            newSubstring = "sinh⁻¹";
                            addParen = true;
                            break;
                        }
                        case "acosh": {
                            newSubstring = "cosh⁻¹";
                            addParen = true;
                            break;
                        }
                        case "acoth": {
                            newSubstring = "coth⁻¹";
                            addParen = true;
                            break;
                        }
                        case "acosech": {
                            newSubstring = "cosech⁻¹";
                            addParen = true;
                            break;
                        }
                        case "asech": {
                            newSubstring = "sech⁻¹";
                            addParen = true;
                            break;
                        }
                        case "cbrt": {
                            newSubstring = "∛";
                            break;
                        }
                    }

                    let underscoreIndex = newSubstring.lastIndexOf("_");
                    if (underscoreIndex != -1) {
                        let subscript = "";
                        for (
                            let i = underscoreIndex + 1;
                            i < newSubstring.length;
                            i++
                        ) {
                            subscript += digitToSubscript(newSubstring[i]);
                        }

                        newSubstring =
                            newSubstring.slice(0, underscoreIndex) + subscript;
                    }

                    offset -= substring.length - newSubstring.length;
                    let parenthesis = "";
                    if (substring.endsWith("(")) {
                        parenthesis = "(";
                        offset += 1;
                    } else if (addParen) {
                        parenthesis = "()";
                        offset += 1;
                    }

                    return `<span style="color: ${identifiercolor}">${newSubstring}</span>${parenthesis}`;
                }

                if (op) {
                    return `<span style="color: ${operatorcolor}">${substring}</span>`;
                }

                return substring;
            }
        );

        result = result.replace(/([_₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎]\d+)/g, (substring) => {
            let newSubstring = substring
                .replace("_", "")
                .replace(/\d/, digitToSubscript);
            offset -= substring.length - newSubstring.length;

            return newSubstring;
        });

        return [result, offset];
    }

    function digitToSubscript(input: string): string {
        switch (input) {
            case "0":
                return "₀";
            case "1":
                return "₁";
            case "2":
                return "₂";
            case "3":
                return "₃";
            case "4":
                return "₄";
            case "5":
                return "₅";
            case "6":
                return "₆";
            case "7":
                return "₇";
            case "8":
                return "₈";
            case "9":
                return "₉";
            case "+":
                return "₊";
            case "-":
                return "₋";
            case "=":
                return "₌";
            case "(":
                return "₍";
            case ")":
                return "₎";
        }

        return input;
    }
</script>

<div class="calculator" bind:this={calculatorElement}>
    <section class="output" bind:this={outputElement}>
        <slot />
        {#each outputLines as line}
            <console-line byuser={line[1]}>
                {#if line[1]}
                    <span style="color: {promptcolor}">&gt;&gt;</span>
                {/if}
                <span class="value">
                    {@html line[0]}
                </span>
            </console-line>
        {/each}
    </section>
    <section class="input-area">
        <span class="prompt" style="color: {promptcolor}">&gt;&gt;&nbsp;</span>
        {#await import("@paddim8/kalk")}
            <span>Loading...</span>
        {:then kalk}
            <div class="input-field-wrapper">
                <div
                    class="highlighted-text"
                    aria-hidden
                    bind:this={highlightedTextElement}
                >
                    <span class="placeholder">{hinttext}</span>
                </div>
                <textarea
                    class="input"
                    placeholder={hinttext}
                    autocomplete="off"
                    autocorrect="off"
                    autocapitalize="off"
                    spellcheck="false"
                    use:handleLoad
                    bind:this={inputElement}
                    on:keydown={(event) => handleKeyDown(event, kalk)}
                    on:keyup={handleKeyUp}
                    on:input={handleInput}
                    role="textbox"
                />
            </div>
        {:catch error}
            <span style="color: {errorcolor}">{error}</span>
        {/await}
    </section>
    {#if buttonpanel}
        <section class="button-panel">
            <button
                class="arrow left"
                on:click={(e) => handleArrowClick(e, true)}>←</button
            >
            {#each buttonRowValues as value}
                <button on:click={handleButtonClick}>{value}</button>
            {/each}
            <button
                class="arrow right"
                on:click={(e) => handleArrowClick(e, false)}>→</button
            >
            {#if numberrow}
                {#each numberRowValues as value}
                    <button on:click={handleButtonClick}>{value}</button>
                {/each}
            {/if}
        </section>
    {/if}
</div>

<style lang="scss">
    .calculator {
        display: flex;
        flex-direction: column;
        width: 100%;
        height: 100%;
        box-sizing: border-box;
        background-color: inherit;
        color: inherit;

        .output {
            display: flex;
            flex-grow: 1;
            flex-direction: column;
            background-color: inherit;
            padding: 10px;
            padding-bottom: 0;
            box-sizing: border-box;
            font-size: 1.4em;
            overflow: auto;
        }

        .output > :first-child {
            margin-top: auto;
        }

        .input-area {
            display: flex;
            background-color: inherit;
            display: flex;
            padding-left: 10px;
            font-size: 1.4em;
            padding-bottom: 10px;
            box-sizing: border-box;
        }

        .placeholder {
            color: gray;
        }

        .prompt {
            background-color: inherit;
        }

        .input-field-wrapper {
            position: relative;
            width: 100%;
        }

        .highlighted-text {
            display: inline-block;
            width: 100%;
            color: white;
            word-wrap: anywhere;
            line-height: 1;
            z-index: 1;
        }

        .input {
            display: inline-block;
            position: absolute;
            top: 0;
            left: 0;
            height: 100%;
            width: 100%;
            border: 0;
            font-size: inherit;
            font-family: inherit;

            line-height: 1;
            overflow: hidden;
            cursor: text;
            color: transparent;
            background: transparent;
            caret-color: white;
            resize: none;
            z-index: 2;
            word-wrap: anywhere;

            &:focus {
                outline: none;
            }
        }

        .button-panel {
            display: grid;
            grid-template-columns: repeat(10, auto);
            grid-template-rows: repeat(3, auto);
        }

        .button-panel {
            width: 100%;

            button {
                $margin: 4px;
                margin-right: $margin;
                margin-bottom: $margin;
                position: relative;
                border: 0;
                padding: 2.5vw;
                font-size: 6.5vw;
                line-height: 1.2;
                background-color: inherit;
                font-family: inherit;
                color: inherit;
                cursor: pointer;
            }

            @media screen and (min-width: 768px) {
                button {
                    padding: 12px;
                    font-size: 1.4em;
                }
            }

            button:after {
                content: "";
                position: absolute;
                left: 0px;
                top: 0px;
                right: 0px;
                bottom: 0px;
                background-color: rgba(0, 0, 0, 0.2);
            }

            button:last-child {
                margin-right: 0;
            }
        }

        .arrow.left {
            grid-area: 1 / 1 / span 2 / 1;
        }

        .arrow.right {
            margin-right: 0;
            grid-area: 1 / 10 / span 2 / 10;
        }
    }
</style>
