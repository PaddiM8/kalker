'use strict';

import { task, src, dest, watch, series } from 'gulp';
import sass, { compiler, logError } from 'gulp-sass';

compiler = require('sass');

task('sass', async () => {
    return src('./www/style/**/*.sass')
        .pipe(sass().on('error', logError))
        .pipe(dest('./www/dist'));
});

task('copyKalkComponent', async () => {
    return src('./node_modules/@paddim8/kalk-component/public/build/*')
        .pipe(dest('./www/dist/'));
});

task('watch', () => {
    watch('./style/**/*.sass', series('sass'));

});

task('default', series('sass', 'copyKalkComponent'));
