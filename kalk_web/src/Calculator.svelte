<script lang="ts">
    let outputLines: string[] = [];
    function handleKeyDown(event: KeyboardEvent) {
        if (event.key == "Enter") {
            const target = event.target as HTMLInputElement;
            outputLines = [...outputLines, target.innerHTML];
            target.innerHTML = "";
        }
    }

    function handleInput(event: Event) {
        const target = event.target as HTMLInputElement;
        const cursorPos = getCursorPos(target);
        const [highlighted, offset] = highlight(target.textContent);
        console.log(highlighted);
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
                    switch (substring) {
                        case "sqrt": {
                            substring = "√";
                            offset += 3;
                            break;
                        }
                        case "sum": {
                            substring = "Σ";
                            offset += 2;
                            break;
                        }
                        case "pi": {
                            substring = "π";
                            offset += 1;
                            break;
                        }
                        case "gamma": {
                            substring = "Γ";
                            offset += 4;
                            break;
                        }
                        case "floor": {
                            substring = "⌊⌋";
                            offset += 3;
                            break;
                        }
                        case "ceil": {
                            substring = "⌈⌉";
                            offset += 3;
                            break;
                        }
                    }

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

<style>
    .calculator {
        width: 600px;
        height: 350px;
    }

    .output {
        display: flex;
        flex-direction: column;
        justify-content: flex-end;
        width: 100%;
        height: 100%;
        background-color: #424242;
        padding: 10px;
        padding-bottom: 0;
        box-sizing: border-box;
        font-size: 1.4em;
        font-family: "Hack", monospace, Consolas, sans-serif; /* TODO: import font */
        color: white;
    }

    .consoleLine {
        margin-top: 0;
        margin-bottom: 0;
    }

    .input-area {
        background-color: #424242;
        display: flex;
        padding-left: 10px;
        font-size: 1.4em;
        padding-bottom: 10px;
    }

    .prompt,
    .input {
        background-color: #424242;
        font-family: "Hack", monospace, Consolas, sans-serif; /* TODO: import font */
    }

    .input {
        display: inline-block;
        width: 100%;
        color: white;
    }
</style>

<div class="calculator">
    <div class="output">
        {#each outputLines as line}
            <p class="consoleLine">
                <span class="prompt">&gt;&gt;</span>
                {@html line}
            </p>
        {/each}
    </div>
    <div class="input-area">
        <span class="prompt">&gt;&gt;&nbsp;</span>
        <div
            contenteditable="true"
            class="input"
            on:keydown={handleKeyDown}
            on:input={handleInput}
            role="textbox" />
    </div>
</div>
