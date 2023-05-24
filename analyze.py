import pandas as pd
import matplotlib.pyplot as plt

# do math
def do_math():
    T1 = 576460619581
    T2 = 576460751519
    iter = 392

    time = T2 - T1
    av = time / iter
    print(av)

def plot(min, max):
    df = pd.read_csv('measures/rot0.csv')
    last_three_columns = df.iloc[:, min:max]
    index = df.iloc[:, 0]
    
    plt.plot(index, last_three_columns.iloc[:, 0], label='Value X')
    plt.plot(index, last_three_columns.iloc[:, 1], label='Value Y')
    plt.plot(index, last_three_columns.iloc[:, 2], label='Value Z')
    plt.title('Value of Over the 3 axis')
    plt.xlabel('Index')
    plt.ylabel('Value')
    plt.legend()
    plt.show()

# plot(2, 5)
# plot(5, 8)
# plot(8, 11)

do_math()