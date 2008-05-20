/* test buffer for C statements */


float extreme(float *values, int n, bool biggest, bool absolute) 
{
    float *trans(float);
    
    if (absolute)
        trans = &fabs;
    else
        trans = &identity;

    float x = values[0];
    for (i = 1; i < n; i++) {
        if (biggest) {
            if ((*trans)(values[i]) > (*trans)(x)) 
                x = values[i];
        } else {
            if ((*trans)(values[i]) < (*trans)(x)) 
                x = values[i];
        }
    }
}

float identity(float x)
{
    return x;
}

