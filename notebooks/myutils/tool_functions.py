from typing import Any, Dict, List, Union

import numpy as np
import pandas as pd


def separate(df: pd.DataFrame, 
             col: List[str], 
             into: List[str], 
             sep: List[str], 
             remove = True) -> pd.DataFrame:
    
    df = df.copy()
    
    df[into] = df[col].str.split(sep, expand = True)
    if remove:
        df.drop(col, axis=1, inplace=True)
    
    return df


def str_replace_colnames(df: pd.DataFrame, 
                         pattern: str, 
                         replacement: str) -> pd.DataFrame:
                         
    df = df.copy()
    
    df.columns = df.columns.str.replace(pattern, replacement)
    
    return df


def rearrange_cols(df: pd.DataFrame, 
                   cols: List[str]) -> pd.DataFrame:
    df = df.copy()
    
    assert(len(df.columns) == len(cols)), "cols length does not match the number of columns of the DataFrame"
    
    df = df[cols]
    
    return df

