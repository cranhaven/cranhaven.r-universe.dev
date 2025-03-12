// bstree.h
// By Pavlo Mozharovskyi
// Last changed 27.05.2014
// Binary search tree

template<typename T> struct bstree_node{
	T value;
	bstree_node *leftChild;
	bstree_node *rightChild;
	bstree_node(T newValue){
		this->value = newValue;
		this->leftChild = 0;
		this->rightChild = 0;
	}
};

template<typename T> class bstree{
public:
	bstree_node<T> *root;
	vector<bstree_node<T>*> nodes;
	bool insert_unique(T newValue);
	void free(){
		if (this->root){
			for (int i = nodes.size() - 1; i > 0; i--){
				delete nodes[i];
			}
			delete this->root;
			this->root = NULL;
			nodes.clear();
		}
	}
	bstree(){
		nodes.resize(0);
		root = NULL;
	}
	~bstree(){
		free();
	}
};

template<typename T> bool bstree<T>::insert_unique(T newValue){
	if (this->root == NULL){
		this->root = new bstree_node<T>(newValue);
		nodes.push_back(this->root);return true;
	}
	bstree_node<T> *curNode = this->root;
	while (curNode != 0){
		if (curNode->value == newValue){
			return false;
		}else{
			if (newValue < curNode->value){
				if (curNode->leftChild == 0){
					curNode->leftChild = new bstree_node<T>(newValue);
					nodes.push_back(curNode->leftChild);
					return true;
				}else{
					curNode = curNode->leftChild;
				}
			}else{
				if (curNode->rightChild == 0){
					curNode->rightChild = new bstree_node<T>(newValue);
					nodes.push_back(curNode->rightChild);
					return true;
				}else{
					curNode = curNode->rightChild;
				}
			}
		}
	}
	return false;
}
