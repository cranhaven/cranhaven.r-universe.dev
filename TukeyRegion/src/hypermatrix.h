// hypermatrix.h
// By Pavlo Mozharovskyi
// Last changed 11.01.2018
// A binary hypermatrix that contains acsendingly indexed cells only as bits

template<typename BlockType> class binaryHypermatrixCmb{
private:
	binaryHypermatrixCmb();
protected:
	int n,d,blockSize;
	BlockType *body;
	unsigned long long size;
public:
	binaryHypermatrixCmb(int, int);
	bool setIfNotSet(vector<int>);
	void free(){
		if (this->body){
			delete[] this->body;
			this->body = NULL;
		}
	}
	~binaryHypermatrixCmb(){
		free();
	}
};

template<typename BlockType> binaryHypermatrixCmb<BlockType>::
	binaryHypermatrixCmb(int n, int d){
	this->n = n;this->d = d;this->blockSize = 8*sizeof(BlockType);
	this->size=choose(n,d)/(blockSize) + 1;
	body = new BlockType[choose(n,d)/blockSize + 1]();
}

template<typename BlockType> bool binaryHypermatrixCmb<BlockType>::
	setIfNotSet(vector<int> index){
	unsigned long long bitNumber = 0;
	int size = index.size();
	for (int i = d - 1; i>=0; i--){
		bitNumber += choose(index[i], i+1);
	}
	unsigned long long blockNumber = bitNumber/blockSize;
	int bitInBlock = bitNumber%blockSize;
	BlockType mask = (BlockType)1 << bitInBlock;
	if (body[blockNumber] & mask){
		return false;
	}else{
		body[blockNumber] |= mask;
		return true;
	}
}
