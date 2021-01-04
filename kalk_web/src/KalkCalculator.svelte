<script lang="ts">
    import { afterUpdate } from "svelte";
    import type { Context } from "@paddim8/kalk";

    export let identifierColor = "cornflowerblue";
    export let operatorColor = "darkorange";
    export let promptColor = "mediumseagreen";
    export let errorColor = "tomato";
    export let hintColor = "#9c9c9c";
    export let linkColor = "cornflowerblue";

    type Kalk = typeof import("@paddim8/kalk");

    let outputLines: [value: string, byUser: boolean][] = [];
    let outputElement: HTMLElement;
    let kalkContext: Context;
    let selectedLineOffset: number = 0;

    afterUpdate(() => {
        // Scroll to bottom
        outputElement.children[
            outputElement.children.length - 1
        ].scrollIntoView(false);
    });

    function calculate(
        kalk: Kalk,
        input: string
    ): [result: string, error: string] {
        try {
            if (!kalkContext) kalkContext = new kalk.Context();
            const result = kalkContext.evaluate(input) ?? "";
            if (result) {
                const sciNot = result.toScientificNotation();
                if (sciNot.exponent > 7 || sciNot.exponent < -6) {
                    return [sciNot.toString(), null];
                }
            }

            return [result.toString(), null];
        } catch (err) {
            return [undefined, err];
        }
    }

    function handleKeyDown(event: KeyboardEvent, kalk: Kalk) {
        if (event.key == "Enter") {
            selectedLineOffset = 0;
            const target = event.target as HTMLInputElement;
            const input = target.textContent;
            let output: string;

            if (input.trim() == "help") {
                output = `<a style="color: ${linkColor}"
                             href="https://kalk.netlify.app/#usage"
                             target="blank">Link to usage guide</a>`;
            } else {
                const [result, err] = calculate(kalk, input);
                output =
                    highlight(result)[0] ??
                    `<span style="color: ${errorColor}">${err}</span>`;
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

    function getCursorPos(element: HTMLInputElement): number {
        const selection = window.getSelection();
        if (selection.rangeCount !== 0) {
            const range = selection.getRangeAt(0);
            const preCaretRange = range.cloneRange();
            preCaretRange.selectNodeContents(element);
            preCaretRange.setEnd(range.endContainer, range.endOffset);
            return preCaretRange.toString().length;
        }

        return 0;
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
        let result = input;
        let offset = 0;
        result = result.replaceAll(
            /(?<identifier>[^!-@\s_|^⌊⌋⌈⌉]+(_\d+)?)|(?<op>[+\-/*%^!])/g,
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
                        case "pi": {
                            newSubstring = "π";
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

                    return `<span style="color: ${identifierColor}">${newSubstring}</span>`;
                }

                if (op) {
                    return `<span style="color: ${operatorColor}">${substring}</span>`;
                }

                return substring;
            }
        );

        if (result.endsWith(" ")) result = result.slice(0, -1) + "&nbsp";

        return [result, offset];
    }
</script>

<style lang="scss">
    .calculator {
        display: flex;
        flex-direction: column;
        width: 100%;
        height: 100%;
        box-sizing: border-box;
        background-color: inherit;
        color: inherit;
    }

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

    .consoleLine {
        margin-top: 0;
        margin-bottom: 5px;
        word-wrap: break-word;
    }

    .result {
        margin-bottom: 10px;
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

        &:focus {
            outline: none;
        }
    }
</style>

<svelte:options tag="kalk-calculator" />
<div class="calculator">
    <div class="output" bind:this={outputElement}>
        <p class="consoleLine">kalk</p>
        <p class="consoleLine">
            <span style="color: {hintColor}">Type 'help' for instructions.</span>
        </p>
        {#each outputLines as line}
            <p class="consoleLine" class:result={!line[1]}>
                {#if line[1]}
                    <span style="color: {promptColor}">&gt;&gt;</span>
                {/if}
                {@html line[0]}
            </p>
        {/each}
    </div>
    <div class="input-area">
        <span class="prompt" style="color: {promptColor}">&gt;&gt;&nbsp;</span>
        {#await import('@paddim8/kalk')}
            <span>Loading...</span>
        {:then kalk}
            <div
                contenteditable="true"
                class="input"
                on:keydown={(event) => handleKeyDown(event, kalk)}
                on:keyup={handleKeyUp}
                on:input={handleInput}
                role="textbox" />
        {:catch error}
            <span style="color: {errorColor}">{error}</span>
        {/await}
    </div>
</div>
