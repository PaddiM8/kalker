<script lang="ts">
    import { afterUpdate } from "svelte";

    let outputLines: string[] = [];
    let outputElement: HTMLElement;
    let kalkContext;

    afterUpdate(() => {
        // Scroll to bottom
        outputElement.children[
            outputElement.children.length - 1
        ].scrollIntoView(false);
    });

    function handleKeyDown(event: KeyboardEvent, kalk) {
        if (event.key == "Enter") {
            const target = event.target as HTMLInputElement;
            const input = target.textContent;
            let output: string;

            if (input.trim() == "help") {
                output =
                    "<a href='https://kalk.netlify.app/#usage' target='blank'>Link to usage guide</a>";
            } else {
                // Calculate
                if (!kalkContext) kalkContext = new kalk.Context();
                try {
                    const result = kalkContext.evaluate(input);
                    if (result) output = result.toString();
                } catch (err) {
                    output = `<span class="error">${err}</span>`;
                }
            }

            const inputHTML = `<span class="prompt">&gt;&gt;&nbsp;</span>${target.innerHTML}`;
            outputLines = output
                ? [...outputLines, inputHTML, output]
                : [...outputLines, inputHTML];

            target.innerHTML = "";
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
                    let newSubstring: string;
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

                    return `<span class="identifier">${substring}</span>`;
                }

                if (op) {
                    return `<span class="operator">${substring}</span>`;
                }

                return substring;
            }
        );

        if (result.endsWith(" ")) result = result.slice(0, -1) + "&nbsp";

        return [result, offset];
    }
</script>

<style lang="scss">
    $background-color: #424242;
    $font: "Hack", monospace, Consolas, sans-serif; /* TODO: import font */

    .calculator {
        width: 600px;
        height: 350px;
    }

    .output {
        display: flex;
        flex-direction: column;
        height: 100%;
        background-color: #424242;
        padding: 10px;
        padding-bottom: 0;
        box-sizing: border-box;
        font-size: 1.4em;
        font-family: $font;
        color: white;
        overflow: auto;
    }

    .output > :first-child {
        margin-top: auto;
    }

    .consoleLine {
        margin-top: 0;
        margin-bottom: 0;
    }

    .input-area {
        background-color: $background-color;
        display: flex;
        padding-left: 10px;
        font-size: 1.4em;
        padding-bottom: 10px;
    }

    .prompt,
    .input {
        background-color: $background-color;
        font-family: $font;
    }

    .input {
        display: inline-block;
        width: 100%;
        color: white;
    }
</style>

<div class="calculator">
    <div class="output" bind:this={outputElement}>
        <p class="consoleLine">kalk</p>
        <p class="consoleLine">
            <span class="hint">Type 'help' for instructions.</span>
        </p>
        {#each outputLines as line}
            <p class="consoleLine">
                {@html line}
            </p>
        {/each}
    </div>
    <div class="input-area">
        <span class="prompt">&gt;&gt;&nbsp;</span>
        {#await import('@paddim8/kalk') then kalk}
            <div
                contenteditable="true"
                class="input"
                on:keydown={(event) => handleKeyDown(event, kalk)}
                on:input={handleInput}
                role="textbox" />
        {/await}
    </div>
</div>
