# Skill: quantmod chart_Series Indicators

When using the newer `chart_Series()` function (as opposed to the older `chartSeries()`), the naming convention for adding technical indicators changes.

## Core Principle
Standard indicators for `chart_Series()` use an underscore prefix: `add_`.

## Supported Standard Indicators
The following indicators have dedicated `add_` functions:
- `add_Vo()`: Volume
- `add_SMA()`: Simple Moving Average
- `add_EMA()`: Exponential Moving Average
- `add_BBands()`: Bollinger Bands
- `add_MACD()`: MACD
- `add_RSI()`: RSI
- `add_ADX()`: ADX

## Unsupported Indicators
If an indicator does not have a direct `add_` version (e.g., SAR, OBV, MFI, CLV), you should use the generic `add_TA()` function:
- `add_TA(SAR(OHLC), on = 1)`: Parabolic SAR
- `add_TA(OBV(Cl, Vo))`: On-Balance Volume
- `add_TA(MFI(HLC, Vo))`: Money Flow Index
- `add_TA(CLV(HLC))`: Close Location Value

## Implementation Example
```r
cs <- chart_Series(data)
cs <- add_Vo()
cs <- add_MACD()
cs <- add_TA(SAR(data), on = 1)
print(cs)
```
