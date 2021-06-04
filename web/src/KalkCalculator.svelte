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
        "*",
        "/",
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
    let hasBeenInteractedWith = false;

    function setText(text: string) {
        const [highlighted, offset] = highlight(text);
        const prevCursorPos = inputElement.selectionStart;
        setHtml(highlighted);
        inputElement.selectionStart = prevCursorPos + offset;
        inputElement.selectionEnd = inputElement.selectionStart;
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

    function calculate(
        kalk: Kalk,
        input: string
    ): [result: string, success: boolean] {
        try {
            if (!kalkContext) kalkContext = new kalk.Context();
            const result = kalkContext.evaluate(input.replaceAll(/\s+/g, " "));

            return [result?.toPrettyString(), true];
        } catch (err) {
            return [err, false];
        }
    }

    function hasUnevenAmountOfBraces(input: string): boolean {
        let openCount = 0;
        let closedCount = 0;
        for (const char of input) {
            if (char == "{") openCount++;
            if (char == "}") closedCount++;
        }

        return openCount > closedCount;
    }

    function handleKeyDown(event: KeyboardEvent, kalk: Kalk) {
        hasBeenInteractedWith = true;
        if (event.key == "Enter") {
            if (
                hasUnevenAmountOfBraces(
                    (event.target as HTMLTextAreaElement).value
                )
            ) {
                return;
            }

            selectedLineOffset = 0;
            const input = inputElement.value;
            let output: string;

            if (input.trim() == "help") {
                output = `<a style="color: ${linkcolor}"
                             href="https://kalker.strct.net/#usage"
                             target="blank">Link to usage guide</a>`;
            } else if (input.trim() == "clear") {
                outputLines = [];
                setText("");
                return;
            } else {
                const [result, success] = calculate(kalk, input);

                output = success
                    ? highlight(result)[0]
                    : `<span style="color: ${errorcolor}">${result}</span>`;
            }

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

    function handleInput(event: Event) {
        const target = event.target as HTMLInputElement;
        setText(target.value == "\n" ? "" : target.value);
    }

    function handleTouchLine(event: Event) {
        if (!inputElement.value) {
            const target = event.currentTarget as HTMLElement;
            setHtml(target.innerHTML);

            // Sighs... What else?
            let i = 0;
            setInterval(() => {
                if (i == 40) return;
                inputElement.focus({ preventScroll: true });
                i++;
            }, 1);
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
        inputElement.selectionEnd = Math.min(Math.max(selection, 0), length);
        inputElement.selectionStart = inputElement.selectionEnd;
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

        inputElement.setRangeText(
            input,
            inputElement.selectionStart,
            inputElement.selectionEnd,
            "end"
        );
        setText(inputElement.value);
        inputElement.selectionStart += offset;
        inputElement.selectionEnd = inputElement.selectionStart;
        inputElement.focus({ preventScroll: true });
    }

    function handleLoad(element: HTMLElement) {
        if (autofocus) element.focus();
    }

    function highlight(input: string): [string, number] {
        if (!input) return ["", 0];
        let result = input;
        let offset = 0;
        result = result.replace(
            /(?<html>[<>&]|(\n\s*\}?|\s+))|(?<op>([+\-/*%^!≈]|if|otherwise)|(?<identifier>[^!-@\s_|^⌊⌋⌈⌉≈\[\]\{\}≠≥≤]+(_\d+)?))/g,
            (substring, _, html, _2, op, identifier) => {
                if (html) {
                    if (substring == "<") return "&lt;";
                    if (substring == ">") return "&gt;";
                    if (substring == "&") return "&amp;";
                    if (substring.startsWith("\n")) {
                        if (substring.endsWith("}")) {
                            return "<br />}";
                        } else {
                            if (!substring.match(/\n\s\s/)) offset += 2;
                            return "<br />&nbsp;&nbsp;";
                        }
                    }
                    if (substring.match(/\s+/)) {
                        return "&nbsp;".repeat(substring.length);
                    }
                }

                if (identifier) {
                    let newSubstring: string = substring;
                    switch (substring) {
                        case "sqrt": {
                            newSubstring = "√";
                            break;
                        }
                        case "sum": {
                            newSubstring = "∑";
                            break;
                        }
                        case "prod": {
                            newSubstring = "∏";
                            break;
                        }
                        case "integrate": {
                            newSubstring = "∫";
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
                    }

                    offset -= substring.length - newSubstring.length;

                    return `<span style="color: ${identifiercolor}">${newSubstring}</span>`;
                }

                if (op) {
                    return `<span style="color: ${operatorcolor}">${substring}</span>`;
                }

                return substring;
            }
        );

        return [result, offset];
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
                <span class="value" on:touchstart={handleTouchLine}>
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
