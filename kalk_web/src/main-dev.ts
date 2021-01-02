import KalkCalculator from './KalkCalculator.svelte';

const app = new KalkCalculator({
	target: document.body
});

export default app;

export {
	KalkCalculator,
}