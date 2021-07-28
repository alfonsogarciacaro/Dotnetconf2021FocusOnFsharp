// Snowpack Configuration File
// See all supported options: https://www.snowpack.dev/reference/configuration

/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
  mount: {
    /* ... */
  },
  plugins: [
    "@snowpack/plugin-react-refresh"
  ],
  packageOptions: {
    external: ["react-native"],
  },
  devOptions: {
    /* ... */
  },
  buildOptions: {
    /* ... */
  },
};
