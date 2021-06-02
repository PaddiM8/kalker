import path from 'path';
import MiniCssExtractPlugin from 'mini-css-extract-plugin';
import OptimizeCSSAssetsPlugin from 'optimize-css-assets-webpack-plugin';
import TerserPlugin from 'terser-webpack-plugin';
import { CleanWebpackPlugin } from 'clean-webpack-plugin';
import Preprocess from 'svelte-preprocess';
import webpack from 'webpack';
import WebpackDevServer from 'webpack-dev-server';

const mode =
	(process.env.NODE_ENV as 'production' | 'development') || 'development';
const prod = mode === 'production';
const sveltePath = path.resolve('node_modules', 'svelte');

/**
 * Should source maps be generated alongside your production bundle? This will expose your raw source code, so it's
 * disabled by default.
 */
const sourceMapsInProduction = false;

/**
 * Should we run Babel on builds? This will transpile your bundle in order to work on your target browsers (see the
 * `browserslist` property in your package.json), but will impact bundle size and build speed.
 */
const useBabel = false;

/**
 * Should we run Babel on development builds? If set to `false`, only production builds will be transpiled. If you're
 * only testing in modern browsers and don't need transpiling in development, it is recommended to keep this disabled
 * as it will greatly speed up your builds.
 */
const useBabelInDevelopment = false;

const config: webpack.Configuration & WebpackDevServer.Configuration = {
	entry: {
		bundle: [
			'./src/main.ts',
		],
	},
	resolve: {
		alias: {
			// Note: Additional aliases will be loaded automatically from `tsconfig.compilerOptions.paths`
			svelte: path.resolve('node_modules', 'svelte'),
		},
		extensions: ['.mjs', '.js', '.ts', '.svelte'],
		mainFields: ['svelte', 'browser', 'module', 'main'],
	},
	output: {
		publicPath: prod ? '' : '/build/',
		path: __dirname + '/public/build',
		filename: '[name].js',
		chunkFilename: '[name].[id].js',
		library: "kalkComponent",
		libraryTarget: 'window',
		libraryExport: 'default'
	},
	module: {
		rules: [
			{
				test: /\.svelte$/,
				use: {
					loader: 'svelte-loader-hot',
					options: {
						customElement: true,
						dev: !prod,
						emitCss: prod,
						hotReload: !prod,
						hotOptions: {
							// List of options and defaults: https://www.npmjs.com/package/svelte-loader-hot#usage
							noPreserveState: false,
							optimistic: true,
						},
						preprocess: Preprocess({
							scss: true,
							postcss: {
								plugins: [require('autoprefixer')],
							},
						}),
					},
				},
			},
			{
				test: /\.(scss|sass)$/,
				use: [
					{
						loader: MiniCssExtractPlugin.loader,
						options: {
							hmr: !prod,
							sourceMap: !prod || sourceMapsInProduction,
						},
					},
					'css-loader',
					{
						loader: 'postcss-loader',
						options: {
							plugins: [require('autoprefixer')],
						},
					},
					'sass-loader',
				],
			},
			{
				test: /\.css$/,
				use: [
					{
						loader: MiniCssExtractPlugin.loader,
						options: {
							hmr: !prod,
							sourceMap: !prod || sourceMapsInProduction,
						},
					},
					'css-loader',
				],
			},
			{
				test: /\.ts$/,
				use: 'ts-loader',
				exclude: /node_modules/,
			},
		],
	},
	devServer: {
		hot: true,
		stats: 'minimal',
		contentBase: 'public',
		watchContentBase: true,
	},
	mode,
	plugins: [
		new MiniCssExtractPlugin({
			filename: '[name].css',
		}),
	],
	optimization: {
		minimizer: [],
	},
	devtool: prod && !sourceMapsInProduction ? false : 'source-map',
};

// Load path mapping from tsconfig
const tsconfigPath = path.resolve(__dirname, 'tsconfig.json');
const tsconfig = require('fs').existsSync(tsconfigPath) ? require(tsconfigPath) : {};

if ('compilerOptions' in tsconfig && 'paths' in tsconfig.compilerOptions) {
	const aliases = tsconfig.compilerOptions.paths;
	for (const alias in aliases) {
		const paths = aliases[alias].map((p: string) => path.resolve(__dirname, p));

		// Our tsconfig uses glob path formats, whereas webpack just wants directories
		// We'll need to transform the glob format into a format acceptable to webpack
		const wpAlias = alias.replace(/(\\|\/)\*$/, '');
		const wpPaths = paths.map((p: string) => p.replace(/(\\|\/)\*$/, ''));

		if (!(wpAlias in config.resolve.alias) && wpPaths.length) {
			config.resolve.alias[wpAlias] = wpPaths.length > 1 ? wpPaths : wpPaths[0];
		}
	}
}

// These options should only apply to production builds
if (prod) {
	// Clean the build directory for production builds
	config.plugins.push(new CleanWebpackPlugin());

	// Minify CSS
	config.optimization.minimizer.push(
		new OptimizeCSSAssetsPlugin({
			cssProcessorOptions: {
				map: sourceMapsInProduction
					? {
						inline: false,
						annotation: true,
					}
					: false,
			},
			cssProcessorPluginOptions: {
				preset: [
					'default',
					{
						discardComments: {
							removeAll: !sourceMapsInProduction,
						},
					},
				],
			},
		})
	);

	// Minify and treeshake JS
	config.optimization.minimizer.push(
		new TerserPlugin({
			sourceMap: sourceMapsInProduction,
			extractComments: false,
		})
	);
}

// Add babel if enabled
if (useBabel && (prod || useBabelInDevelopment)) {
	config.module.rules.unshift({
		test: /\.(?:svelte|m?js)$/,
		include: [path.resolve(__dirname, 'src'), path.dirname(sveltePath)],
		use: {
			loader: 'babel-loader',
			options: {
				sourceType: 'unambiguous',
				presets: ['@babel/preset-env'],
				plugins: ['@babel/plugin-transform-runtime'],
			},
		},
	});
}

export default config;
