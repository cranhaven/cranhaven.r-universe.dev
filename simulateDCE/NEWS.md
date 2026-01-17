# simulateDCE 0.3.1


## Major Changes:
 * speed improvements to simulate_choices function, which affects the sim_all function as well.


## Bug fixes
 * removed 'estimate' argument from simulate_choices as it was not used.


# simulateDCE 0.3.0

## Major Changes:
- **Improved `sim_all` function**:
  - Returns additional outputs: beta parameters, utility functions, manipulations, and decision group arguments.
  - Parallel processing partly implemented
  - Allows to save chunks of data to disk, which is useful for large datasets and instable machines when simulations are large
  - New functions for better modularity

# simulateDCE 0.2.0

## Enhancements:
- **`readdesign` function**: 
  - Can now automatically guess the `designtype` (useful for `sim_all` with mixed `ngene` and `spdesign` design files).
- **`sim_all` function**: 
  - Returns more information, including beta parameters and decision-related arguments.

# simulateDCE 0.1.2

## Initial Release:
- First stable version. Includes fully working examples.
- Can be used by anyone.
