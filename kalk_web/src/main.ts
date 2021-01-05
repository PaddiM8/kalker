import "./public-path.js";
import KalkCalculator from './KalkCalculator.svelte';
import ConsoleLine from './ConsoleLine.svelte';
import component from "svelte-tag";

new component({
	component: KalkCalculator,
	tagname: "kalk-calculator",
	attributes: [
		"identifiercolor",
		"operatorcolor",
		"promptcolor",
		"errorcolor",
		"linkcolor",
		"hinttext",
		"autofocus"
	]
});

new component({
	component: ConsoleLine,
	tagname: "console-line",
	attributes: ["byuser"]
});

export {
	KalkCalculator,
	ConsoleLine,
}