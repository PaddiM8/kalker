import { defineConfig } from 'vite'
import { svelte } from '@sveltejs/vite-plugin-svelte'
import wasm from "vite-plugin-wasm";
import topLevelAwait from "vite-plugin-top-level-await";
import { resolve } from "path";


// https://vitejs.dev/config/
export default defineConfig({
    build: {
        lib: {
            entry: resolve(__dirname, 'src/main.ts'),
            name: 'Kalker-component',
            formats: ["es"],
            fileName: `kalker`
        },
        rollupOptions: {
            output: {
                inlineDynamicImports: true
            }
        }

    },

    plugins: [svelte(
        {
            compilerOptions: {
                customElement: true,
            }
        }
    ),
    wasm(), topLevelAwait(),
    ]
})