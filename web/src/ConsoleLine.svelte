<script lang="ts">
    export let byuser = false;
</script>

<style lang="scss">
    p {
        margin-top: 0;
        margin-bottom: 5px;
        word-wrap: break-word;
    }

    .result {
        margin-bottom: 10px;
    }
</style>

<svelte:options tag="console-line" />
<p class:result={!byuser}>
    <slot />
</p>
