{ pkgs } : [

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libiconv-1.16-2-any.pkg.tar.zst";
  sha256 = "0nr8gaqz7vhjsqq8ys3z63bd62fz548r9n0sncz513ra04wg7la4";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-zlib-1.2.11-9-any.pkg.tar.zst";
  sha256 = "0fb3xbw9a0ah4viwp3a7hr5phnc7mvcl9ba2yjidncpqmspypacx";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-binutils-2.38-2-any.pkg.tar.zst";
  sha256 = "121jz2nmfk0qgkwjll8bg3kavmzpp14raid4az44p10vfdlla7f6";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-headers-git-9.0.0.6451.a3f6d363d-1-any.pkg.tar.zst";
  sha256 = "1ngnjb9vgk295wlwqandm0nhqqdfrp584kx3nfshxkhwmj5gpzxk";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-crt-git-9.0.0.6451.a3f6d363d-1-any.pkg.tar.zst";
  sha256 = "1ccipidbsjncdhr48k50ia53dwn7v3ghdl8f1svgwvnh3mrx0bww";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-isl-0.24-1-any.pkg.tar.zst";
  sha256 = "0dngp6p1yw3i9mvwg9rl888dqa7fjs8xczx1lqacw7lj98q1396d";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-gmp-6.2.1-3-any.pkg.tar.zst";
  sha256 = "170640c8j81gl67kp85kr8kmg5axsl1vqwn9g7cx6vcr638qax9c";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-mpfr-4.1.0.p13-1-any.pkg.tar.zst";
  sha256 = "17klcf17mddd7hsrak920zglqh00drqjdh6dxh3v3c4y62xj1qr6";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-mpc-1.2.1-1-any.pkg.tar.zst";
  sha256 = "0761i6aga4982v6mw1hgqrrqznki0c8v93xkpf5fqmsjysfncscc";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libwinpthread-git-9.0.0.6451.a3f6d363d-1-any.pkg.tar.zst";
  sha256 = "0qdy79l5y02lw2xa8i3j6yayhz8a7awfgyyd82pcmbzwx57q2xqb";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-gcc-libs-11.2.0-10-any.pkg.tar.zst";
  sha256 = "1n8q09dwh0ghaw3p3bgxi3q0848gsjzd210bgp7qy05hv73b8kc1";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-windows-default-manifest-6.4-4-any.pkg.tar.zst";
  sha256 = "1ylipf8k9j7bgmwndkib2l29mds394i7jcij7a6ciag4kynlhsvi";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-winpthreads-git-9.0.0.6451.a3f6d363d-1-any.pkg.tar.zst";
  sha256 = "08zwgkrp45y5ry8avz61krasjkk4k4a5rrdz4nd78bbbah84mpgz";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-zstd-1.5.2-2-any.pkg.tar.zst";
  sha256 = "1f14wbc1yvjgv3rbwhv75391l55gcm0as6ipba20vw8phz4ax8ds";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-gcc-11.2.0-10-any.pkg.tar.zst";
  sha256 = "0h6pi1xrxbrg27klbj5i5rjl8ydz78lpjfhb9pdayxjr8rs5nblq";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-c-ares-1.18.1-1-any.pkg.tar.zst";
  sha256 = "13j7zx9773k0gx3wbqq38jkcndyjpnm7dfb85i8q2dda27g4iq2m";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-brotli-1.0.9-4-any.pkg.tar.zst";
  sha256 = "0vn42aqam6m9755vy32qr626xqglb6vsbmywdyvvagzmm8s5fxrg";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-expat-2.4.7-1-any.pkg.tar.zst";
  sha256 = "19qh3kk2kmkkzxirpx1swgfsfb29gy9q8qgdmrzzbwrqrn8vs77j";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-gettext-0.21-3-any.pkg.tar.zst";
  sha256 = "1gy7fmn6jc13ipnyyq44gyhv8rvz5cy7gz1dm3wrna80hjnzli5v";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libunistring-0.9.10-4-any.pkg.tar.zst";
  sha256 = "0fimgakzgxrnnqk5rkvnbwl6lyqdrpgl2jcbcagqjv1swdmav97m";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libidn2-2.3.1-1-any.pkg.tar.zst";
  sha256 = "00vm6d56ldr1f4h0dn15j0ja17dif45qxlxaqv4x5nw555frklf5";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libpsl-0.21.1-4-any.pkg.tar.zst";
  sha256 = "083nng8zis1v2lshnqymxnalprr8g6gdwf84il5ys1ga90pi6bbn";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libtasn1-4.18.0-1-any.pkg.tar.zst";
  sha256 = "0lr1c33d2mkm51kq027bxcj2735vk3nndmn8g02d2v73h6akl48k";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libffi-3.3-4-any.pkg.tar.zst";
  sha256 = "0iswfiql785ngavdz3qdxahj6wn531j5cwij945gbr9q6wbav0bi";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-p11-kit-0.24.1-2-any.pkg.tar.zst";
  sha256 = "1rx1sjhda4g42qsvmw9nvpdk14ag67sxgfiydivg55hxqjxsvk9p";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-ca-certificates-20210119-1-any.pkg.tar.zst";
  sha256 = "0ia48njn92shddlq3f707wz6dvj0j1k60iscs6vybybw33ijhgsq";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-openssl-1.1.1.n-1-any.pkg.tar.zst";
  sha256 = "0gb6nswwm4b66w4x2zydha23fck967hzb5gckwlv05dws13hbh22";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libssh2-1.10.0-1-any.pkg.tar.zst";
  sha256 = "1f27an41hxrfs9jifq0708c484ps3zmb582gmsy7xn5idg3wk03d";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-jansson-2.14-2-any.pkg.tar.zst";
  sha256 = "0hwvcyp7mcvljii87mv0d467whr5j8i8rjkkam7r784qrp9i49ds";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-jemalloc-5.2.1-2-any.pkg.tar.zst";
  sha256 = "04lf1b1sdbb8ncbimbb9q0lv7qlc3s814p2001zsxa2dhdc4xdri";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-xz-5.2.5-2-any.pkg.tar.zst";
  sha256 = "1w12cbn6zq2szfa1wgr019i4ayv9x29d1nnh5vlq26l6fmzd3j1g";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libxml2-2.9.13-1-any.pkg.tar.zst";
  sha256 = "093fm3i8018mig0wy2p53z8izm0bfqlviacray79j2kv1bq8xyn6";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-nghttp2-1.47.0-1-any.pkg.tar.zst";
  sha256 = "0lwylch8s7blr2hdngs2v0syh1gnicm0z4wpi28ifwdp4kzrh7mh";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-curl-7.82.0-1-any.pkg.tar.zst";
  sha256 = "0czysrplb3lgd59v4c4v8sihbcs3hdih9d8iqrhkf28n7vds859i";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-rust-1.59.0-1-any.pkg.tar.zst";
  sha256 = "1c1yr7w3h2ybbx6cywqgpns4ii0dws7jqcix8fiv0rbkjq5hxlsv";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-pkgconf-1.8.0-2-any.pkg.tar.zst";
  sha256 = "18gzbyc949rvaisdxrf4lyx349xigzpp4dk5a9jj9ghn8zfa1wlg";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-jsoncpp-1.9.4-2-any.pkg.tar.zst";
  sha256 = "0xqckav97gsaazdfn4395jz0ma0i3snvs1g4ghb7s5jsxbwrhr82";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-bzip2-1.0.8-2-any.pkg.tar.zst";
  sha256 = "1kqg3aw439cdyhnf02rlfr1pw1n8v9xxvq2alhn7aw6nd8qhw7z5";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libb2-0.98.1-2-any.pkg.tar.zst";
  sha256 = "1nj669rn1i6fxrwmsqmr9n49p34wxvhn0xlsn9spr6aq1hz73b41";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-lz4-1.9.3-1-any.pkg.tar.zst";
  sha256 = "0fxvabi93cxfybbn49hlr3wgzs4p7fw5shfa055222apkxnncm92";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libtre-git-r128.6fb7206-2-any.pkg.tar.xz";
  sha256 = "0dp3ca83j8jlx32gml2qvqpwp5b42q8r98gf6hyiki45d910wb7x";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libsystre-1.0.1-4-any.pkg.tar.xz";
  sha256 = "037gkzaaj8kp5nspcbc8ll64s9b3mj8d6m663lk1za94bq2axff1";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-nettle-3.7.3-3-any.pkg.tar.zst";
  sha256 = "10p1jlik2zhqnpphk3k1q6k2my6j0zig49r5fs21a8f6l0gakj1x";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libarchive-3.6.0-1-any.pkg.tar.zst";
  sha256 = "1jd0rj49il09a56dnvgyjzmjj37pdspqhkfm8smgakgsgn9wkm46";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libuv-1.42.0-3-any.pkg.tar.zst";
  sha256 = "0mg4j5lqmxlhgrs9bnkb1bhj3mfpvjvvkzpjyy87y2m2k11ffbja";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-rhash-1.4.2-1-any.pkg.tar.zst";
  sha256 = "0yhv8pra83cs0mk0n40w0k12z32slxs88h1p9z2ixvyigf8w86ml";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-cmake-3.22.3-2-any.pkg.tar.zst";
  sha256 = "1xp8n5s98va7a9cq36d9p49lk32yv9vkvlayc4d4j465xzm21hgp";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-ninja-1.10.2-3-any.pkg.tar.zst";
  sha256 = "1wsylz9m3hm6lq71qfyc3hc8rmxnv3kp602d1yvvh8nvb8pgza1y";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-mpdecimal-2.5.1-1-any.pkg.tar.zst";
  sha256 = "0cpyacmciyzbsar1aka5y592g2gpa4i6a58j3bjdmfjdnpm0j08a";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-ncurses-6.3-3-any.pkg.tar.zst";
  sha256 = "1kicmq9xy4mh5rzzf107jikpmql0h78618b3xpks5l59c1hm8ril";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-termcap-1.3.1-6-any.pkg.tar.zst";
  sha256 = "1wgbzj53vmv1vm3igjan635j5ims4x19s2y6mgvvc46zgndc2bvq";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-readline-8.1.001-1-any.pkg.tar.zst";
  sha256 = "0q04sz2ibvcd69hb681j4s6zyakm4i7zpk12qajj38l17v9qmlac";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-tcl-8.6.11-5-any.pkg.tar.zst";
  sha256 = "0f3p6x55d0370khpp77xpr1dwhfhrlb8b1wjxxb96y0x67q1casm";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-sqlite3-3.38.1-1-any.pkg.tar.zst";
  sha256 = "04h4m72mwmad82nzrl5qj9wlsinjs7z7bsbbq17dxkx92aj75p26";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-tk-8.6.11.1-2-any.pkg.tar.zst";
  sha256 = "0awr7hzxliyvrkh0ywrga69lcnl5g41i7d4w4azhdwk7i60i1s40";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-tzdata-2022a-1-any.pkg.tar.zst";
  sha256 = "0z1q4359q5vfs77a9wnhmf2i9y3ldfmpijjgzqv4za1grmyj6whd";
})

(pkgs.fetchurl {
  url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-python-3.9.11-2-any.pkg.tar.zst";
  sha256 = "05znxaybrm8affs83a51872iksa9yd4qk692lr3rjdjy3cbxkhca";
})
]
