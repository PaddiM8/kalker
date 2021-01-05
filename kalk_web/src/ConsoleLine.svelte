<script lang="ts">
    export let byuser = false;
</script>

<style lang="scss">
    .console-line-tg638d3 {
        margin-top: 0;
        margin-bottom: 5px;
        word-wrap: break-word;
    }

    .console-line-tg638d3.result {
        margin-bottom: 10px;
    }
</style>

<p class="console-line-tg638d3" class:result={!byuser}>
    <slot />
</p>
