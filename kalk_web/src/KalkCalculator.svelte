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
        "Σ",
        "⌊",
        "⌈",
        "∫",
    ];
    let numberRowValues = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"];
    let outputElement: HTMLElement;
    let kalkContext: Context;
    let selectedLineOffset: number = 0;
    let calculatorElement: HTMLElement;
    let inputElement: HTMLInputElement;

    afterUpdate(() => {
        // Scroll to bottom
        outputElement.children[
            outputElement.children.length - 1
        ].scrollIntoView(false);
    });

    function calculate(
        kalk: Kalk,
        input: string
    ): [result: string, success: boolean] {
        try {
            if (!kalkContext) kalkContext = new kalk.Context();
            const result = kalkContext.evaluate(input);

            return [result?.toPrettyString(), true];
        } catch (err) {
            return [err, false];
        }
    }

    function handleKeyDown(event: KeyboardEvent, kalk: Kalk) {
        if (event.key == "Enter") {
            selectedLineOffset = 0;
            const target = event.target as HTMLInputElement;
            const input = target.textContent;
            let output: string;

            if (input.trim() == "help") {
                output = `<a style="color: ${linkcolor}"
                             href="https://kalk.netlify.app/#usage"
                             target="blank">Link to usage guide</a>`;
            } else {
                const [result, success] = calculate(
                    kalk,
                    input.replace(/\s+/g, "") // Temporary fix, since it for some reason complains about spaces on chrome
                );

                output = success
                    ? highlight(result)[0]
                    : `<span style="color: ${errorcolor}">${result}</span>`;
            }

            outputLines = output
                ? [...outputLines, [target.innerHTML, true], [output, false]]
                : [...outputLines, [target.innerHTML, true]];

            target.innerHTML = "";
        }
    }

    function handleKeyUp(event: KeyboardEvent) {
        // on keyup, since pressing the arrow up key makes the cursor go to the start
        // of the input field. This piece of code will put the cursor at the end,
        // which therefore will need to be done afterwards, so that it doesn't just get moved back again.
        if (event.key == "ArrowUp" || event.key == "ArrowDown") {
            const target = event.target as HTMLInputElement;
            const change = event.key == "ArrowUp" ? 1 : -1;
            selectedLineOffset += change;

            if (selectedLineOffset < 0) {
                target.innerHTML = "";
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
                target.innerHTML = line[0];
                setCursorPosEnd(target);
            }

            if (selectedLineOffset >= outputLines.length) {
                selectedLineOffset = outputLines.length - 1;
            }
        }
    }

    function handleInput(event: Event) {
        const target = event.target as HTMLInputElement;
        const cursorPos = getCursorPos(target);
        const [highlighted, offset] = highlight(target.textContent);
        target.innerHTML = highlighted;
        setCursorPos(target, cursorPos - offset);
    }

    function handleTouchLine(event: Event) {
        if (!inputElement.innerHTML) {
            const target = event.currentTarget as HTMLElement;
            inputElement.innerHTML = target.querySelector(".value").innerHTML;
        }
    }

    function handleButtonClick(event: Event) {
        const target = event.target as HTMLElement;
        target.blur();
        insertText(target.textContent);
    }

    function handleArrowClick(event: Event, left: boolean) {
        const target = event.target as HTMLElement;
        const cursorPos = getCursorPos(inputElement);
        target.blur();
        setCursorPos(inputElement, cursorPos + (left ? -1 : 1));
    }

    function insertText(input: string) {
        let cursorPos = getCursorPos(inputElement);
        const textContent = inputElement.textContent;
        let movementOffset = input.length;

        if (input == "(") {
            input += ")";
        } else if (input == "=") {
            input = " = ";
            movementOffset = 3;
        } else if (input == "Σ") {
            input += "()";
            movementOffset = 2;
        } else if (input == "∫") {
            input += "()";
            movementOffset = 2;
        } else if (input == "⌊") {
            input += "⌋";
        } else if (input == "⌈") {
            input += "⌉";
        } else if (input == ",") {
            input = ", ";
            movementOffset = 2;
        }

        const newString =
            textContent.slice(0, cursorPos) +
            input +
            textContent.slice(cursorPos);
        const [highlighted, offset] = highlight(newString);

        inputElement.innerHTML = highlighted;
        inputElement.focus();
        setCursorPos(inputElement, cursorPos - offset + movementOffset);
    }

    function focus(element: HTMLInputElement) {
        if (autofocus) element.focus();
    }

    function getCursorPos(element: HTMLInputElement): number {
        const shadowRoot = calculatorElement.getRootNode() as ShadowRoot;
        const range = shadow.getRange(shadowRoot);
        //const selection = shadowRoot.getSelection();
        //const range = selection.getRangeAt(0);
        const preCaretRange = range.cloneRange();
        preCaretRange.selectNodeContents(element);
        preCaretRange.setEnd(range.endContainer, range.endOffset);

        return preCaretRange.toString().length;
    }

    function setCursorPos(element: HTMLElement, indexToSelect: number) {
        const range = document.createRange();
        range.selectNodeContents(element);
        const textNodes = getTextNodesIn(element);

        let nodeEndPos = 0;
        for (let i = 0; i < textNodes.length; i++) {
            const textNode = textNodes[i];
            const previousNodeEndPos = nodeEndPos;
            nodeEndPos += textNode.length;

            // If the index that should be selected is
            // less than or equal to the current position (the end of the text node),
            // then the index points to somewhere inside the current text node.
            // This text node along with indexToSelect will then be used when setting the cursor position.
            if (indexToSelect <= nodeEndPos) {
                range.setStart(textNode, indexToSelect - previousNodeEndPos);
                range.setEnd(textNode, indexToSelect - previousNodeEndPos);
                break;
            }
        }

        const selection = window.getSelection();
        selection.removeAllRanges();
        selection.addRange(range);
    }

    function setCursorPosEnd(element: HTMLElement) {
        const range = document.createRange();
        const selection = window.getSelection();
        range.selectNodeContents(element);
        range.setStart(element, range.endOffset);
        selection.removeAllRanges();
        selection.addRange(range);
    }

    function getTextNodesIn(node: Node): Text[] {
        const textNodes: Text[] = [];

        // If it's text node, add it to the list directly,
        // otherwise go through it recursively and find text nodes within it.
        if (node.nodeType == Node.TEXT_NODE) {
            textNodes.push(node as Text);
        } else {
            for (const child of node.childNodes) {
                textNodes.push(...getTextNodesIn(child));
            }
        }

        return textNodes;
    }

    function highlight(input: string): [string, number] {
        if (!input) return ["", 0];
        let result = input;
        let offset = 0;
        result = result.replace(
            /(?<identifier>[^!-@\s_|^⌊⌋⌈⌉≈]+(_\d+)?)|(?<op>[+\-/*%^!≈])/g,
            (substring, identifier, _, op) => {
                if (identifier) {
                    let newSubstring: string = substring;
                    switch (substring) {
                        case "sqrt": {
                            newSubstring = "√";
                            break;
                        }
                        case "sum": {
                            newSubstring = "Σ";
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

                    offset += substring.length - newSubstring.length;

                    return `<span style="color: ${identifiercolor}">${newSubstring}</span>`;
                }

                if (op) {
                    return `<span style="color: ${operatorcolor}">${substring}</span>`;
                }

                return substring;
            }
        );

        if (result.endsWith(" ")) result = result.slice(0, -1) + "&nbsp";

        return [result, offset];
    }
</script>

<div class="calculator" bind:this={calculatorElement}>
    <section class="output" bind:this={outputElement}>
        <slot />
        {#each outputLines as line}
            <console-line byuser={line[1]} on:touchstart={handleTouchLine}>
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
            <div
                type="text"
                contenteditable="true"
                class="input"
                placeholder={hinttext}
                autocomplete="off"
                autocorrect="off"
                autocapitalize="off"
                spellcheck="false"
                use:focus
                bind:this={inputElement}
                on:keydown={(event) => handleKeyDown(event, kalk)}
                on:keyup={handleKeyUp}
                on:input={handleInput}
                role="textbox"
            />
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
            background-color: inherit;
            display: flex;
            padding-left: 10px;
            font-size: 1.4em;
            padding-bottom: 10px;
        }

        .prompt,
        .input {
            background-color: inherit;
        }

        .input {
            display: inline-block;
            width: 100%;
            color: white;
            word-wrap: anywhere;
            cursor: text;

            &:focus {
                outline: none;
            }
        }

        [contenteditable][placeholder]:empty:before {
            content: attr(placeholder);
            position: absolute;
            color: gray;
            background-color: transparent;
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
