<script lang="ts">
    export let byUser = false;
</script>

<style>
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
<p class:result={!byUser}>
    <slot />
</p>
